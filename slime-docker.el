;;; slime-docker.el --- Integration of SLIME with Docker containers. -*- lexical-binding: t; -*-

;; URL: https://github.com/daewok/slime-docker
;; Package-Requires: ((emacs "24") (slime "2.16") (docker-tramp "0.1") (cl-lib "0.5"))
;; Keywords: docker, lisp, slime
;; Version: 0.8


;;; License:

;;  GPLv2+

;;  Copyright (c) 2016 Eric Timmons

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the Free
;;  Software Foundation; either version 2 of the License, or (at your option)
;;  any later version.

;;  This program is distributed in the hope that it will be useful, but WITHOUT
;;  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;  more details.

;;  You should have received a copy of the GNU General Public License along with
;;  this program; if not, write to the Free Software Foundation, Inc., 51
;;  Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


;;; Commentary:

;; slime-docker provides an easy bridge between SLIME and Lisps running in
;; Docker containers.  It can launch a container from an image, start a Lisp,
;; connect to it using SLIME, and set up filename translations (if the
;; slime-tramp contrib is enabled).
;;
;; To get started, describe the Lisp implementations and Docker images you want
;; to use in the variable `slime-docker-implementations'.  Then, run
;; `slime-docker' and away you go.
;;
;; The default image used by this package is daewok/lisp-devel:latest
;; (https://hub.docker.com/r/daewok/lisp-devel/)
;;
;; SLIME is hard to use directly with Docker containers because its
;; initialization routine is not very flexible.  It requires that both Lisp and
;; Emacs have access to the same filesystem (so the port Swank is listening on
;; can be shared) and that the port Swank listens on is the same port to which
;; SLIME has to connect.  Neither of these are necessarily true with Docker.
;;
;; This works around this by watching the stdout of the Lisp process to figure
;; out when Swank is ready to accept connections.  It also queries the Docker
;; daemon to determine which port 4005 has been forwarded to.

;;; Code:

(require 'slime)
(require 'docker-tramp)
(require 'cl-lib)


;;;; Variable Definitions

(defvar slime-docker-implementations nil
  "A list of known Lisp implementations running on Docker.
The list should have the form:
  ((NAME (PROGRAM PROGRAM-ARGS ...) &key KEYWORD-ARGS) ...)

NAME is a symbol for the implementation.

PROGRAM and PROGRAM-ARGS are strings used to start
the Lisp
process inside the Docker container.

For KEYWORD-ARGS see `slime-docker-start'")

(defvar slime-docker-default-lisp nil
  "The name of the default Lisp implementation for `slime-docker'.
See `slime-docker-implementations'")

(defvar slime-docker--cid nil
  "A buffer local variable in the inferior proccess.")

(defvar slime-docker--inferior-lisp-program-history '()
  "History list of command strings.  Used by `slime-docker'.")

(defvar slime-docker--path nil
  "Directory containing the slime-docker package.
The default value is automatically computed from the location of
the Emacs Lisp package.")
(setq slime-docker--path (file-name-directory load-file-name))

(defgroup slime-docker nil
  "The slime-docker group."
  :group 'slime)

(defcustom slime-docker-ensure-mount-folders-exist t
  "If non-NIL, ensure that folders that are mounted into a Docker
  container exist before starting the container. This ensures
  those folders are created owned by the current user instead of
  root (which is the case if docker has to make the folder)."
  :type 'boolean
  :group 'slime-docker)

(defvar slime-docker-machine-ssh-agent-helper-path nil
  "The location of the docker-run-ssh-agent-helper script.
This script is used to help share an SSH-Agent between the host
computer and a docker container running on docker-machine.
The default value is automatically computed.")

(defun slime-docker-find-ssh-agent-helper ()
  (cond
   ((file-exists-p (concat slime-docker--path "bin/docker-run-ssh-agent-helper"))
    (concat slime-docker--path "bin/docker-run-ssh-agent-helper"))
   ((file-exists-p (concat slime-docker--path "docker-run-ssh-agent-helper"))
    (concat slime-docker--path "docker-run-ssh-agent-helper"))
   (t
    nil)))

(setq slime-docker-machine-ssh-agent-helper-path
      (slime-docker-find-ssh-agent-helper))

(defvar slime-docker-sbcl-seccomp-profile nil
  "The location of the seccomp profile for SBCL (the default
docker seccomp plus allowing the use of personality to disable
ASLR.")

(defun slime-docker--find-sbcl-seccomp-profile ()
  "Attempt to find the seccomp profile for SBCL."
  (cond
   ((file-exists-p (concat slime-docker--path "resources/docker-sbcl-seccomp.json"))
    (concat slime-docker--path "resources/docker-sbcl-seccomp.json"))
   (t
    nil)))

(setq slime-docker-sbcl-seccomp-profile
      (slime-docker--find-sbcl-seccomp-profile))


;;;; Docker machine integration
(defun slime-docker--machine-get-env-string (machine)
  "Get the env string for MACHINE from docker-machine."
  (shell-command-to-string
   (format "docker-machine env --shell=sh %S" machine)))

(defun slime-docker--machine-variables-alist (machine)
  "Get the environment variables for MACHINE from docker-machine.

Returns an alist."
  (let ((env-string (slime-docker--machine-get-env-string machine))
        (count 1)
        (out nil))
    (while (string-match "^\\(export .*=.*\\)$" env-string)
      (let ((subexpr (match-string 1 env-string)))
        (save-match-data
          (unless (string-match "^export \\(.*\\)=\"\\(.*\\)\"$" subexpr)
            (error "format of environment variable from `docker-machine env' different than expected."))
          (push (cons (match-string 1 subexpr) (match-string 2 subexpr))
                out))
        (setq env-string (replace-match "" nil nil env-string 1))))
    out))

(defun slime-docker--machine-variables-string (machine)
  "Get the environment variables for MACHINE from docker-machine.

Returns a list of strings suitable for use with
`process-environment'."
  (mapcar (lambda (x) (concat (car x) "=" (cdr x)))
          (slime-docker--machine-variables-alist machine)))

(defun slime-docker--get-process-environment (args)
  "Get the `process-environment' to run Docker in."
  (cl-destructuring-bind (&key docker-machine docker-machine-setenv &allow-other-keys)
      args
    (cond
     ((and docker-machine docker-machine-setenv)
      (mapc (lambda (x) (setenv (car x) (cdr x)))
            (slime-docker--machine-variables-alist docker-machine))
      process-environment)
     (docker-machine
      (append (slime-docker--machine-variables-string docker-machine)
              process-environment))
     (t
      process-environment))))

(defun slime-docker--machine-ip (machine)
  "Get the IP of MACHINE from docker-machine."
  (replace-regexp-in-string "\n\\'" ""
                            (shell-command-to-string
                             (concat "docker-machine ip " machine))))


;;;; Constructing Docker Containers

(defun slime-docker--sanitize-pathname (pathname)
  "If on Windows, sanitize PATHNAME by returning what the path would be in the docker machine."
  (cond ((string-equal system-type "windows-nt")
         (unless (string-match "^.\\(:\\)/.*" pathname)
           (error "Unable to sanitize %s" pathname))
         (concat "/" (replace-match "" nil t pathname 1)))
        (t pathname)))

(defun slime-docker--mount-to-arg (mount)
  "Convert a MOUNT description to a Docker argument.

Given a mount description of the form:

\((HOST-PATH . CONTAINER-PATH) &key READ-ONLY)

return the argument that should be passed to docker run to mount this volume."
  (cl-destructuring-bind ((host-vol . container-vol) &key read-only)
      mount
    (let ((base-string (format "--volume=%s:%s"
                               (slime-docker--sanitize-pathname host-vol)
                               container-vol)))
      (when read-only
        (setq base-string (concat base-string ":ro")))
      base-string)))

(defun slime-docker--env-to-arg (e)
  "Convert E, a pair, to a Docker argument.

Given an environment description of the form

\(VARIABLE . VALUE)

return the argument that should be passed to docker run to set variable to value."
  (cl-destructuring-bind (var . val) e
    (concat "--env=" var "=" val)))

(defun slime-docker--port-to-arg (p)
  "Convert P, a plist, to a Docker argument.

Recognized properties are :ip, :host-port, and :container-port."
  (cl-destructuring-bind (&key host-port ip container-port) p
    (concat "--publish="
            (if ip
                (concat ip ":")
              "")
            (if host-port
                (if (listp host-port)
                    (format "%s-%s" (car host-port) (cdr host-port))
                  (format "%s" host-port)))
            (if (or ip host-port)
                ":"
              "")
            (if container-port
                (if (listp container-port)
                    (format "%s-%s" (car container-port) (cdr container-port))
                  (format "%s" container-port))))))

(defun slime-docker--security-opt-to-arg (e)
  "Convert E, a pair, to a Docker argument.

Given an environment description of the form

\(SECURITY-OPTION . VALUE)

return the argument that should be passed to docker run to set the security option."
  (cl-destructuring-bind (var . val) e
    (concat "--security-opt=" var "=" val)))

(defun slime-docker---cid (proc)
  "Given a Docker PROC, return the container ID."
  (with-current-buffer (process-buffer proc)
    slime-docker--cid))

(defun slime-docker--port (proc args)
  "Given a Docker PROC, return the port that 4005 is mapped to."
  (cl-destructuring-bind (&key docker-command &allow-other-keys) args
    (let* ((process-environment (slime-docker--get-process-environment args))
           (port-string (shell-command-to-string
                         (format "%s port %S 4005" docker-command (slime-docker---cid proc)))))
      (cl-assert (string-match ".*:\\([0-9]*\\)$" port-string)
                 "Unable to determine external port number.")
      (string-to-number (match-string 1 port-string)))))

(defun slime-docker--make-docker-args (args)
  "Given the user specified arguments, return a list of arguments to be passed to Docker to start a container."
  (cl-destructuring-bind (&key program program-args
                               cid-file
                               image-name image-tag
                               rm mounts env directory
                               uid
                               docker-machine
                               security-opts
                               userns
                               dns
                               ports
                               &allow-other-keys) args
    `("run"
      "-i"
      ,(concat "--cidfile=" cid-file)
      "-p" ,(concat (if docker-machine "" "127.0.0.1::") "4005")
      ,(format "--rm=%s" (if rm "true" "false"))
      ,@(mapcar #'slime-docker--mount-to-arg mounts)
      ,@(mapcar #'slime-docker--env-to-arg env)
      ,@(mapcar #'slime-docker--security-opt-to-arg security-opts)
      ,@(mapcar #'slime-docker--port-to-arg ports)
      ,@(when uid
          (list (format "--user=%s" uid)))
      ,@(when directory
          (list (format "--workdir=%s" directory)))
      ,@(when userns
          (list (format "--userns=%s" userns)))
      ,@(when dns
          (if (listp dns)
              (mapcar (lambda (x) (format "--dns=%s" x))
                      dns)
            (list (format "--dns=%s" dns))))
      ,(format "%s:%s" image-name image-tag)
      ,program
      ,@program-args)))

(defun slime-docker--read-cid (cid-file)
  "Given a CID-FILE where a continer ID has been written, read the container ID from it."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents cid-file)
      (buffer-string))))

(defun slime-docker--ensure-mount-folder-exists (mount-description)
  "Ensures the host folder in requested MOUNT-DESCRIPTION exists."
  (cl-destructuring-bind ((host-vol . container-vol) &rest opts)
      mount-description
    (make-directory host-vol t)))

(defun slime-docker--ensure-mount-folders-exist (args)
  "Ensures that all host folders in requested mounts of ARGS exist."
  (cl-destructuring-bind (&key mounts
                               &allow-other-keys) args
    (mapc #'slime-docker--ensure-mount-folder-exists mounts)))

(defun slime-docker--start-docker (buffer args)
  "Start a Docker container in the given buffer.  Return the process."
  (cl-destructuring-bind (&key docker-command &allow-other-keys) args
    (with-current-buffer (get-buffer-create buffer)
      (comint-mode)
      (erase-buffer)
      (let ((process-connection-type nil)
            (cid-file (make-temp-file "slime-docker"))
            (process-environment (slime-docker--get-process-environment args)))
        (delete-file cid-file)
        (when slime-docker-ensure-mount-folders-exist
          (slime-docker--ensure-mount-folders-exist args))
        (comint-exec (current-buffer) "docker-lisp" docker-command nil
                     (slime-docker--make-docker-args (cl-list* :cid-file cid-file args)))
        (make-local-variable 'slime-docker--cid)
        ;; Wait for cid-file to exist.
        (while (not (file-exists-p cid-file))
          (sit-for 0.1))
	(let ((cid (slime-docker--read-cid cid-file)))
	  (while (string-equal "" cid)
	    (sit-for 0.1)
	    (setq cid (slime-docker--read-cid cid-file)))
	  (setq slime-docker--cid cid)))
      (lisp-mode-variables t)
      (let ((proc (get-buffer-process (current-buffer))))
        ;; TODO: deal with closing process when exiting?
        ;; TODO: Run hooks
        proc))))

(defun slime-docker--maybe-start-docker (args)
  "Return a new or existing docker process."
  (cl-destructuring-bind (&key buffer &allow-other-keys) args
    (cond
     ((not (comint-check-proc buffer))
      (slime-docker--start-docker buffer args))
     ;; TODO: Prompt user to see if the existing process should be reinitialized.
     (t
      (slime-docker--start-docker (generate-new-buffer-name buffer)
                                 args)))))


;;;; Tramp Integration

(defun slime-docker--hostname (proc)
  "Given a Docker PROC, return its hostname."
  (substring (slime-docker---cid proc) 0 12))

(defun slime-docker--translate-filename->emacs (lisp-filename mounts hostname)
  "Translate LISP-FILENAME to a filename that Emacs can open.

MOUNTS is the mounts description that Docker was started with.

HOSTNAME is the hostname of the Docker container."
  ;; First, find the matching mount.
  (let ((matching-mount
         (cl-find-if (lambda (x) (string-match (concat "^" (cdr (car x))) lisp-filename))
                     mounts)))
    (if matching-mount
        (replace-match (car (car matching-mount)) nil t lisp-filename)
      ;; else, fall back to TRAMP
      (tramp-make-tramp-file-name "docker" nil hostname lisp-filename))))

(defun slime-docker--translate-filename->lisp (emacs-filename mounts)
  "Translate the EMACS-FILENAME into a filename that Lisp can open.

MOUNTS is the mounts description that Docker was started with."
  ;; First, find the matching mount.
  (let ((matching-mount
         (cl-find-if (lambda (x) (string-match (concat "^" (car (car x))) emacs-filename))
                     mounts)))
    (if matching-mount
        (replace-match (cdr (car matching-mount)) nil t emacs-filename)
      ;; else, fall back to TRAMP
      (if (tramp-tramp-file-p emacs-filename)
          (tramp-file-name-localname
           (tramp-dissect-file-name emacs-filename))
        "/dev/null"))))




(defun slime-docker--init-command (args)
  "Return a string to initialize Lisp."
  (cl-destructuring-bind (&key slime-mount-path &allow-other-keys)
      args
    (let ((loader (if (file-name-absolute-p slime-backend)
                      slime-backend
                    (concat slime-mount-path "/" slime-backend))))
      (format "%S\n\n"
              `(progn
                 (load ,loader
                       :verbose t)
                 (funcall (read-from-string "swank-loader:init"))
                 (setf (symbol-value (read-from-string "swank::*loopback-interface*")) "0.0.0.0")
                 (funcall (read-from-string "swank:create-server")))))))

(defun slime-docker--start-swank-server (proc args)
  "Start a swank server in Docker PROC.

ARGS are the arguments `slime-docker-start' was called with."
  (cl-destructuring-bind (&key init &allow-other-keys) args
    (with-current-buffer (process-buffer proc)
      (let ((str (funcall init args)))
        (goto-char (process-mark proc))
        (insert-before-markers str)
        (process-send-string proc str)))))

(defun slime-docker--poll-stdout (proc _retries attempt)
  "Return T when swank is ready for connections.

Get the PROC buffer contents, and try to find the string:
';; Swank started at port: [number].'

ATTEMPT is an integer describing which attempt we are on."
  (unless (active-minibuffer-window)
    (message "Polling Lisp stdout for Swank start message .. %d (Abort with `M-x slime-abort-connection'.)"
             attempt))
  (with-current-buffer (process-buffer proc)
    (let ((match (string-match-p ";; Swank started at port: 4005." (buffer-string))))
      (when match
        (message "match: %S" match)
        t))))

(defun slime-docker--connected-hook-function ()
  "A function that is run once SLIME is connected.

Unsets the inferior process for the connection once all other
hooks have run.  Needed to work around `slime-quit-lisp' killing
its inferior buffer, which doesn't give docker time to remove the
container."
  (let* ((c (slime-connection))
         (proc (slime-inferior-process c)))
    (when (slime-docker---cid proc)
      (slime-set-inferior-process c nil))))

(add-hook 'slime-connected-hook 'slime-docker--connected-hook-function t)

(defun slime-docker--connect-when-ready (proc retries attempt args)
  "Connect to SWANK when it is ready for connections.

Checks Lisp's stdout in PROC to see if SWANK is ready.  If it is,
connects.

Otherwise, if there are RETRIES remaining, schedules itself to be
run again in the future.

ATTEMPT is a number saying which attempt this is.

ARGS are the arguments `slime-docker-start' was called with."
  (slime-cancel-connect-retry-timer)
  (cl-destructuring-bind (&key docker-machine mounts &allow-other-keys) args
    (let ((result (slime-docker--poll-stdout proc retries attempt))
          (try-again-p t))
      (cond
       ((numberp result)
        (setq retries result))
       (result
        (setq try-again-p nil)
        (sit-for 0.2)
        (let* ((ip (if docker-machine (slime-docker--machine-ip docker-machine) "127.0.0.1"))
               (c (slime-connect ip (slime-docker--port proc args)))
               (hostname (slime-docker--hostname proc)))
          (slime-set-inferior-process c proc)
          (push (list (concat "^" hostname "$")
                      (lambda (emacs-filename)
                        (slime-docker--translate-filename->lisp emacs-filename mounts))
                      (lambda (lisp-filename)
                        (slime-docker--translate-filename->emacs lisp-filename mounts hostname)))
                slime-filename-translations)))
       ((and retries (zerop retries))
        (setq try-again-p nil)
        (message "Gave up connection to Swank after %d attempts." attempt))
       ((eq (process-status proc) 'exit)
        (setq try-again-p nil)
        (message "Failed to connect to Swank: inferior process exited.")))
      (when try-again-p
        (setq slime-connect-retry-timer
              (run-with-timer 0.3 nil
                              #'slime-timer-call #'slime-docker--connect-when-ready
                              proc (and retries (1- retries)) (1+ attempt)
                              args))))))

(defun slime-docker--connect (proc args)
  "Start SWANK in PROC and connect to it.

INIT is a function that returns the string to start SWANK.

MOUNTS is the mounts description Docker was started with."
  (slime-docker--start-swank-server proc args)
  (slime-docker--connect-when-ready proc nil 0 args))

(defun slime-docker--canonicalize-mounts (mounts)
  (mapcar (lambda (x)
            (cl-list* (cons (expand-file-name (car (first x)))
                            (cdr (first x)))
                      (rest x)))
          mounts))

;;;; User interaction

;;;###autoload
(cl-defun slime-docker-start (&key (program inferior-lisp-program) program-args
                                   directory
                                   name
                                   (buffer "*docker-lisp*")
                                   (image-name "daewok/lisp-devel")
                                   (image-tag "latest")
                                   (rm t)
                                   env
                                   (init 'slime-docker--init-command)
                                   mounts
                                   coding-system
                                   (slime-mount-path "/usr/local/share/common-lisp/source/slime/")
                                   (slime-mount-read-only t)
                                   uid
                                   docker-machine
                                   (docker-command "docker")
                                   (docker-machine-setenv t)
                                   security-opts
                                   userns
                                   dns
                                   ports)
  "Start a Docker container and Lisp process in the container then connect to it.

If the slime-tramp contrib is also loaded (highly recommended),
this will also set up the appropriate tramp translations to view
and edit files in the spawned container.

PROGRAM and PROGRAM-ARGS are the filename and argument strings
  for the Lisp process.
IMAGE-NAME is a string naming the image that should be used to
  start the container.
IMAGE-TAG is a string nameing the tag to use. Defaults to
  \"latest\".
INIT is a function that should return a string to load and start
  Swank. The function will be called with a plist of all
  arguments passed to `slime-docker-start'
CODING-SYSTEM is ignored.
ENV an alist of environment variables to set in the docker
  container.
BUFFER the name of the buffer to use for the subprocess.
NAME a symbol to describe the Lisp implementation.
DIRECTORY set this as the working directory in the container.
RM if true, the container is removed when the process closes.
MOUNTS a list describing the voluments to mount into the
  container. It is of the form:
  (((HOST-PATH . CONTAINER-PATH) &key READ-ONLY) ... )
UID if specified, sets the UID of the Lisp process in the
  container.
SLIME-MOUNT-PATH the location where to mount SLIME into the
  container defaults to
  /usr/local/share/common-lisp/source/slime/
SLIME-MOUNT-READ-ONLY if non-NIL, SLIME is mounted into the
  container as read-only. Defaults to T.
DOCKER-MACHINE if non-NIL, must be a string naming a machine name
  known to docker-machine. If provided, used to set appropriate
  environment variables for the docker process to communicate
  with the desired machine. Does not start the machine if it is
  currently not running.
DOCKER-COMMAND is the command to use when interacting with
  docker. Defaults to \"docker\". See
  `slime-docker-machine-ssh-agent-helper-path' if you are using
  docker-machine and would like to share your SSH Agent with the
  container.
DOCKER-MACHINE-SETENV if non-NIL, uses `setenv' to set Emacs
  environment with the necessary variables from
  docker-machine. Should be non-NIL if you expect tramp to work
  with images running in docker machine.
SECURITY-OPTS specifies --security-opt options when running
  'docker run'. Must be an alist where keys and values are
  strings. See README for note on using this with SBCL.
USERNS specifies the user namespace to use when starting the
  container. See the --userns option to 'docker run' for more
  information.
DNS specifies a list of DNS servers to use in the container. If
  you're on a laptop, it's recommended to set this value as
  Docker does not update a container's DNS info while it is
  running (for example if you change networks).
PORTS is a list of port specifications to open in the docker
  container. The port specifications are plists with the
  properties :ip, :host-port, and :container-port. :ip must be a
  string. :host-port and :container-port must be a number or a
  cons cell."
  (let* ((mounts (cl-list* `((,slime-path . ,slime-mount-path) :read-only ,slime-mount-read-only)
                           mounts))
         (args (list :program program :program-args program-args
                     :directory directory :name name :buffer buffer
                     :image-name image-name :image-tag image-tag
                     :rm rm :env env :init init
                     :mounts (slime-docker--canonicalize-mounts mounts)
                     :slime-mount-path slime-mount-path
                     :slime-read-only slime-mount-read-only
                     :uid uid
                     :docker-machine docker-machine
                     :docker-machine-setenv (and docker-machine docker-machine-setenv)
                     :docker-command docker-command
                     :security-opts security-opts
                     :userns userns
                     :dns dns
                     :ports ports))
         (proc (slime-docker--maybe-start-docker args)))
    (pop-to-buffer (process-buffer proc))
    (slime-docker--connect proc args)))

(defun slime-docker-start* (options)
  "Convenience to run `slime-docker-start' with OPTIONS."
  (apply #'slime-docker-start options))

(defun slime-docker--lisp-options (&optional name)
  (let ((slime-lisp-implementations slime-docker-implementations)
        (slime-default-lisp slime-docker-default-lisp))
    (slime-lisp-options name)))

(defun slime-docker--read-interactive-args ()
  "Return the list of args which should be passed to `slime-docker-start'.

The rules for selecting the arguments are rather complicated:

- In the most common case, i.e. if there's no `prefix-arg' in
  effect and if `slime-docker-implementations' is nil, use
  `inferior-lisp-program' as fallback.

- If the table `slime-docker-implementations' is non-nil use the
  implementation with name `slime-docker-default-lisp' or if
  that's nil the first entry in the table.

- If the `prefix-arg' is `-', prompt for one of the registered
  lisps.

- If the `prefix-arg' is positive, read the command to start the
  process."
  (let ((table slime-docker-implementations))
    (cond ((not current-prefix-arg) (slime-docker--lisp-options))
          ((eq current-prefix-arg '-)
           (let ((key (completing-read
                       "Lisp name: " (mapcar (lambda (x)
                                               (list (symbol-name (car x))))
                                             table)
                       nil t)))
             (slime-lookup-lisp-implementation table (intern key))))
          (t
           (cl-destructuring-bind (program &rest program-args)
               (split-string-and-unquote
                (read-shell-command "Run lisp: " inferior-lisp-program
                                    'slime-docker--inferior-lisp-program-history))
             (let ((coding-system
                    (if (eq 16 (prefix-numeric-value current-prefix-arg))
                        (read-coding-system "set slime-coding-system: "
                                            slime-net-coding-system)
                      slime-net-coding-system)))
               (list :program program :program-args program-args
                     :coding-system coding-system)))))))

;;;###autoload
(defun slime-docker (&optional command)
  "Launch a Lisp process in a Docker container and connect SLIME to it.

The normal entry point to slime-docker.el. Similar to `slime'
function. Tries to guess the correct Lisp to start based on
prefix arguments and the values of `slime-docker-implementations'
and `slime-docker-default-lisp'.

COMMAND is the command to run in the Docker container."
  (interactive)
  (let ((inferior-lisp-program (or command inferior-lisp-program)))
    (slime-docker-start* (cond ((and command (symbolp command))
                                (slime-docker--lisp-options command))
                               (t (slime-docker--read-interactive-args))))))

(provide 'slime-docker)

;;; slime-docker.el ends here
