;;; slime-docker.el --- Integration of SLIME with Docker containers. -*- lexical-binding: t; -*-

;; URL: https://github.com/daewok/slime-docker
;; Package-Requires: ((emacs "24") (slime "2.16") (docker-tramp "0.1") (cl-lib "0.5"))
;; Keywords: docker, lisp, slime
;; Version: 0.2


;;; License:

;;   The MIT License (MIT)
;;
;;   Copyright (c) 2016 Eric Timmons
;;
;;   Permission is hereby granted, free of charge, to any person obtaining a
;;   copy of this software and associated documentation files (the "Software"),
;;   to deal in the Software without restriction, including without limitation
;;   the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;   and/or sell copies of the Software, and to permit persons to whom the
;;   Software is furnished to do so, subject to the following conditions:
;;
;;   The above copyright notice and this permission notice shall be included in
;;   all copies or substantial portions of the Software.
;;
;;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;   DEALINGS IN THE SOFTWARE.


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
  "*A list of known Lisp implementations running on Docker.
The list should have the form:
  ((NAME (PROGRAM PROGRAM-ARGS ...) &key KEYWORD-ARGS) ...)

NAME is a symbol for the implementation.

PROGRAM and PROGRAM-ARGS are strings used to start 
the Lisp
process inside the Docker container.

For KEYWORD-ARGS see `slime-docker-start'")

(defvar slime-docker-cid nil
  "A buffer local variable in the inferior proccess.")

(defvar slime-docker-inferior-lisp-program-history '()
  "History list of command strings.  Used by `slime-docker'.")


;;;; Constructing Docker Containers

(defun slime-docker-sanitize-pathname (pathname)
  "If on Windows, sanitize PATHNAME by returning what the path would be in the docker machine."
  (cond ((string-equal system-type "windows-nt")
         (unless (string-match "^.\\(:\\)/.*" pathname)
           (error "Unable to sanitize %s" pathname))
         (concat "/" (replace-match "" nil t pathname 1)))
        (t pathname)))

(defun slime-docker-mount-to-arg (mount)
  "Convert a MOUNT description to a Docker argument.

Given a mount description of the form:

\((HOST-PATH . CONTAINER-PATH) &key READ-ONLY)

return the argument that should be passed to docker run to mount this volume."
  (cl-destructuring-bind ((host-vol . container-vol) &key read-only)
      mount
    (let ((base-string (format "--volume=%s:%s" (slime-docker-sanitize-pathname host-vol) container-vol)))
      (when read-only
        (setq base-string (concat base-string ":ro")))
      base-string)))

(defun slime-docker-env-to-arg (e)
  "Convert E, a pair, to a Docker argument.

Given an environment description of the form

\(VARIABLE . VALUE)

return the argument that should be passed to docker run to set variable to value."
  (cl-destructuring-bind (var . val) e
    (concat "--env=" var "=" val)))

(defun slime-docker--cid (proc)
  "Given a Docker PROC, return the container ID."
  (with-current-buffer (process-buffer proc)
    slime-docker-cid))

(defun slime-docker-port (proc)
  "Given a Docker PROC, return the port that 4005 is mapped to."
  (let ((port-string (shell-command-to-string
                      (format "docker port %S 4005" (slime-docker--cid proc)))))
    (cl-assert (string-match ".*:\\([0-9]*\\)$" port-string)
               "Unable to determine external port number.")
    (string-to-number (match-string 1 port-string))))

(defun slime-docker-make-docker-args (args)
  "Given the user specified arguments, return a list of arguments to be passed to Docker to start a container."
  (cl-destructuring-bind (&key program program-args
                               cid-file
                               image-name image-tag
                               rm mounts env directory
                               uid
                               &allow-other-keys) args
    `("run"
      "-i"
      ,(concat "--cidfile=" cid-file)
      "-p" "127.0.0.1::4005"
      ,(format "--rm=%s" (if rm "true" "false"))
      ,@(mapcar #'slime-docker-mount-to-arg mounts)
      ,@(mapcar #'slime-docker-env-to-arg env)
      ,@(when uid
          (list (format "--user=%s" uid)))
      ,@(when directory
          (list (format "--workdir=%s" directory)))
      ,(format "%s:%s" image-name image-tag)
      ,program
      ,@program-args)))

(defun slime-docker-read-cid (cid-file)
  "Given a CID-FILE where a continer ID has been written, read the container ID from it."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents cid-file)
      (buffer-string))))


(defun slime-docker-start-docker (buffer args)
  "Start a Docker container in the given buffer.  Return the process."
  (with-current-buffer (get-buffer-create buffer)
    (comint-mode)
    (erase-buffer)
    (let ((process-connection-type nil)
          (cid-file (make-temp-file "slime-docker")))
      (delete-file cid-file)
      (comint-exec (current-buffer) "docker-lisp" "docker" nil
                   (slime-docker-make-docker-args (cl-list* :cid-file cid-file args)))
      (make-local-variable 'slime-docker-cid)
      ;; Wait for cid-file to exist.
      (while (not (file-exists-p cid-file))
        (sit-for 0.1))
      (sit-for 0.5)
      (setq slime-docker-cid (slime-docker-read-cid cid-file)))
    (lisp-mode-variables t)
    (let ((proc (get-buffer-process (current-buffer))))
      ;; TODO: deal with closing process when exiting?
      ;; TODO: Run hooks
      proc)))

(defun slime-docker-maybe-start-docker (args)
  "Return a new or existing docker process."
  (cl-destructuring-bind (&key buffer &allow-other-keys) args
    (cond
     ((not (comint-check-proc buffer))
      (slime-docker-start-docker buffer args))
     ;; TODO: Prompt user to see if the existing process should be reinitialized.
     (t
      (slime-docker-start-docker (generate-new-buffer-name buffer)
                                 args)))))


;;;; Tramp Integration

(defun slime-docker-hostname (proc)
  "Given a Docker PROC, return its hostname."
  (substring (slime-docker--cid proc) 0 12))

(defun slime-docker-translate-filename->emacs (lisp-filename mounts hostname)
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

(defun slime-docker-translate-filename->lisp (emacs-filename mounts)
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




(defun slime-docker-init-command ()
  "Return a string to initialize Lisp."
  (let ((loader (if (file-name-absolute-p slime-backend)
                    slime-backend
                  ;;(concat slime-path slime-backend)
                  (concat "/usr/local/share/common-lisp/source/slime/" slime-backend))))
    (format "%S\n\n"
            `(progn
               (load ,(expand-file-name loader)
                     :verbose t)
               (funcall (read-from-string "swank-loader:init"))
               (setf (symbol-value (read-from-string "swank::*loopback-interface*")) "0.0.0.0")
               (funcall (read-from-string "swank:create-server"))))))

(defun slime-docker-start-swank-server (proc init)
  "Start a swank server in Docker PROC.

INIT is a functio that generates the string to start SWANK."
  (with-current-buffer (process-buffer proc)
    (let ((str (funcall init)))
      (goto-char (process-mark proc))
      (insert-before-markers str)
      (process-send-string proc str))))

(defun slime-docker-poll-stdout (proc _retries attempt)
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

(defun slime-docker-connected-hook-function ()
  "A function that is run once SLIME is connected.

Unsets the inferior process for the connection once all other
hooks have run.  Needed to work around `slime-quit-lisp' killing
its inferior buffer, which doesn't give docker time to remove the
container."
  (let* ((c (slime-connection))
         (proc (slime-inferior-process c)))
    (when (slime-docker--cid proc)
      (message "In docker hook.")
      (slime-set-inferior-process c nil))))

(add-hook 'slime-connected-hook 'slime-docker-connected-hook-function t)

(defun slime-docker-connect-when-ready (proc retries attempt mounts)
  "Connect to SWANK when it is ready for connections.

Checks Lisp's stdout in PROC to see if SWANK is ready.  If it is,
connects.

Otherwise, if there are RETRIES remaining, schedules itself to be
run again in the future.

ATTEMPT is a number saying which attempt this is.

MOUNTS is the mounts description Docker was started with."
  (slime-cancel-connect-retry-timer)
  (let ((result (slime-docker-poll-stdout proc retries attempt))
        (try-again-p t))
    (cond
     ((numberp result)
      (setq retries result))
     (result
      (setq try-again-p nil)
      (sit-for 0.2)
      (let ((c (slime-connect "127.0.0.1" (slime-docker-port proc)))
            (hostname (slime-docker-hostname proc)))
        (slime-set-inferior-process c proc)
        (push (list (concat "^" hostname "$")
                    (lambda (emacs-filename)
                      (slime-docker-translate-filename->lisp emacs-filename mounts))
                    (lambda (lisp-filename)
                      (slime-docker-translate-filename->emacs lisp-filename mounts hostname)))
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
                            #'slime-timer-call #'slime-docker-connect-when-ready
                            proc (and retries (1- retries)) (1+ attempt)
                            mounts)))))

(defun slime-docker-connect (proc init mounts)
  "Start SWANK in PROC and connect to it.

INIT is a function that returns the string to start SWANK.

MOUNTS is the mounts description Docker was started with."
  (slime-docker-start-swank-server proc init)
  (slime-docker-connect-when-ready proc nil 0 mounts))


;;;; User interaction

(cl-defun slime-docker-start (&key (program inferior-lisp-program) program-args
                                   directory
                                   name
                                   (buffer "*docker-lisp*")
                                   (image-name "daewok/lisp-devel")
                                   (image-tag "latest")
                                   (rm t)
                                   env
                                   (init 'slime-docker-init-command)
                                   mounts
                                   coding-system
                                   (slime-mount-path "/usr/local/share/common-lisp/source/slime/")
                                   (slime-mount-read-only t)
                                   uid)
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
  Swank. The function will be called with no arguments - but that
  may change in a future version.
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
  container as read-only. Defaults to T."
  (let* ((args (list :program program :program-args program-args
                     :directory directory :name name :buffer buffer
                     :image-name image-name :image-tag image-tag
                     :rm rm :env env :init init :mounts mounts
                     :slime-mount-path slime-mount-path
                     :slime-read-only slime-mount-read-only
                     :uid uid))
         (mounts (cl-list* `((,slime-path . ,slime-mount-path) :read-only ,slime-mount-read-only)
                           mounts))
         (proc (slime-docker-maybe-start-docker args)))
    (pop-to-buffer (process-buffer proc))
    (slime-docker-connect proc init mounts)))

(defun slime-docker-start* (options)
  "Convenience to run `slime-docker-start' with OPTIONS."
  (apply #'slime-docker-start options))

(defun slime-docker-read-interactive-args ()
  "Return the list of args which should be passed to `slime-docker-start'.

The rules for selecting the arguments are rather complicated:

- In the most common case, i.e. if there's no `prefix-arg' in
  effect and if `slime-docker-implementations' is nil, use
  `inferior-lisp-program' as fallback.

- If the table `slime-docker-implementations' is non-nil use the
  implementation with name `slime-default-lisp' or if
  that's nil the first entry in the table.

- If the `prefix-arg' is `-', prompt for one of the registered
  lisps.

- If the `prefix-arg' is positive, read the command to start the
  process."
  (let ((table slime-docker-implementations))
    (cond ((not current-prefix-arg) (slime-lisp-options))
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
                                    'slime-docker-inferior-lisp-program-history))
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

COMMAND is the command to run in the Docker container."
  (interactive)
  (let ((inferior-lisp-program (or command inferior-lisp-program)))
    (slime-docker-start* (cond ((and command (symbolp command))
                                (slime-lisp-options command))
                               (t (slime-docker-read-interactive-args))))))

(provide 'slime-docker)

;;; slime-docker.el ends here
