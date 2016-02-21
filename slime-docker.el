;; -*- lexical-binding: t; -*-

(require 'slime)
(require 'docker-tramp)


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
  (cond ((string-equal system-type "windows-nt")
         (unless (string-match "^.\\(:\\)/.*" pathname)
           (error "Unable to sanitize %s" pathname))
         (concat "/" (replace-match "" nil t pathname 1)))
        (t pathname)))

(defun slime-docker-mount-to-arg (mount)
  "Given a mount description of the form

((HOST-PATH . CONTAINER-PATH) &key READ-ONLY)

return the argument that should be passed to docker run to mount this volume."
  (cl-destructuring-bind ((host-vol . container-vol) &key read-only)
      mount
    (let ((base-string (format "--volume=%s:%s" (slime-docker-sanitize-pathname host-vol) container-vol)))
      (when read-only
        (setq base-string (concat base-string ":ro")))
      base-string)))

(defun slime-docker-env-to-arg (e)
  "Given an environment description of the form

(VARIABLE . VALUE)

return the argument that should be passed to docker run to set variable to value."
  (cl-destructuring-bind (var . val) e
    (concat "--env=" var "=" val)))

(defun slime-docker--cid (proc)
  "Given a Docker process, return the container ID."
  (with-current-buffer (process-buffer proc)
    slime-docker-cid))

(defun slime-docker-port (proc)
  "Given a Docker process, return the port that 4005 is mapped to."
  (let ((port-string (shell-command-to-string
                      (format "docker port %S 4005" (slime-docker--cid proc)))))
    (cl-assert (string-match ".*:\\([0-9]*\\)$" port-string)
               "Unable to determine external port number.")
    (string-to-number (match-string 1 port-string))))

(defun slime-docker-make-docker-args (program program-args
                                              cid-file
                                              image-name image-tag
                                              rm mounts env directory
                                              uid)
  "Given the user specified arguments, return a list of arguments
to be passed to Docker to start a container."
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
    ,@program-args))

(defun slime-docker-read-cid (cid-file)
  "Given a file where a continer ID has been written, read the
container ID from it."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents cid-file)
      (buffer-string))))


(defun slime-docker-start-docker (program program-args
                                          buffer
                                          image-name image-tag
                                          rm mounts env directory
                                          uid)
  "Start a Docker container in the given buffer. Return the
process."
  (with-current-buffer (get-buffer-create buffer)
    (comint-mode)
    (erase-buffer)
    (let ((process-connection-type nil)
          (cid-file (make-temp-file "slime-docker")))
      (delete-file cid-file)
      (comint-exec (current-buffer) "docker-lisp" "docker" nil
                   (slime-docker-make-docker-args program program-args
                                                  cid-file
                                                  image-name image-tag
                                                  rm mounts env directory
                                                  uid))
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

(defun slime-docker-maybe-start-docker (program program-args
                                                buffer
                                                image-name image-tag
                                                rm mounts env directory
                                                uid)
  "Return a new or existing docker process."
  (cond
   ((not (comint-check-proc buffer))
    (slime-docker-start-docker program program-args
                               buffer
                               image-name image-tag
                               rm mounts env directory
                               uid))
   ;; TODO: Prompt user to see if the existing process should be reinitialized.
   (t
    (slime-docker-start-docker program program-args
                               (generate-new-buffer-name buffer)
                               image-name image-tag
                               rm mounts env directory
                               uid))))


;;;; Tramp Integration

(defun slime-docker-hostname (proc)
  (substring (slime-docker--cid proc) 0 12))

(defun slime-docker-translate-filename->emacs (hostname mounts lisp-filename)
  ;; First, find the matching mount.
  (let ((matching-mount
         (find-if (lambda (x) (string-match (concat "^" (cdr (first x))) lisp-filename))
                  mounts)))
    (if matching-mount
        (replace-match (car (first matching-mount)) nil t lisp-filename)
      ;; else, fall back to TRAMP
      (tramp-make-tramp-file-name "docker" nil hostname lisp-filename))))

(defun slime-docker-translate-filename->lisp (mounts emacs-filename)
  ;; First, find the matching mount.
  (let ((matching-mount
         (find-if (lambda (x) (string-match (concat "^" (car (first x))) emacs-filename))
                  mounts)))
    (if matching-mount
        (replace-match (cdr (first matching-mount)) nil t emacs-filename)
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
  (with-current-buffer (process-buffer proc)
    (let ((str (funcall init)))
      (goto-char (process-mark proc))
      (insert-before-markers str)
      (process-send-string proc str))))

(defun slime-docker-poll-stdout (proc retries attempt)
  "Get the process buffer contents, and try to find the string:
';; Swank started at port: [number].'"
  (unless (active-minibuffer-window)
    (message "Polling Lisp stdout for Swank start message .. %d (Abort with `M-x slime-abort-connection'.)"
             attempt))
  (with-current-buffer (process-buffer proc)
    (let ((match (string-match-p ";; Swank started at port: [0-9]*." (buffer-string))))
      (when match
        (message "match: %S" match)
        t))))

(defun slime-docker-connect-when-ready (proc retries attempt mounts)
  (slime-cancel-connect-retry-timer)
  (let ((result (slime-docker-poll-stdout proc retries attempt))
        (try-again-p t))
    (cond
     ((numberp result)
      (setq retries result))
     (result
      (setq try-again-p nil)
      (let ((c (slime-connect "127.0.0.1" (slime-docker-port proc)))
            (hostname (slime-docker-hostname proc)))
        (slime-set-inferior-process c proc)
        (push (list (concat "^" hostname "$")
                    (lambda (emacs-filename)
                      (slime-docker-translate-filename->lisp mounts emacs-filename))
                    (lambda (lisp-filename)
                      (slime-docker-translate-filename->emacs hostname mounts lisp-filename)))
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
  (let* ((mounts (list* `((,slime-path . ,slime-mount-path) :read-only ,slime-mount-read-only)
                        mounts))
         (proc (slime-docker-maybe-start-docker program program-args
                                               buffer
                                               image-name image-tag
                                               rm mounts env directory
                                               uid)))
    (pop-to-buffer (process-buffer proc))
    (slime-docker-connect proc init mounts)))

(defun slime-docker-start* (options)
  (apply #'slime-docker-start options))

(defun slime-docker (&optional command)
  (interactive)
  (let ((inferior-lisp-program (or command inferior-lisp-program)))
    (slime-docker-start* (cond ((and command (symbolp command))
                                (slime-lisp-options command))
                               (t (slime-docker-read-interactive-args))))))


(defun slime-docker-read-interactive-args ()
  "Return the list of args which should be passed to `slime-docker-start'.

The rules for selecting the arguments are rather complicated:

- In the most common case, i.e. if there's no prefix-arg in
  effect and if `slime-docker-implementations' is nil, use
  `inferior-lisp-program' as fallback.

- If the table `slime-docker-implementations' is non-nil use the
  implementation with name `slime-default-lisp' or if
  that's nil the first entry in the table.

- If the prefix-arg is `-', prompt for one of the registered
  lisps.

- If the prefix-arg is positive, read the command to start the
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

(provide 'slime-docker)
