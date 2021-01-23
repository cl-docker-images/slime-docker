[![MELPA](http://melpa.org/packages/slime-docker-badge.svg)](http://melpa.org/#/slime-docker) [![MELPA Stable](http://stable.melpa.org/packages/slime-docker-badge.svg)](http://stable.melpa.org/#/slime-docker)
# slime-docker.el #

This emacs package is designed to easily integrate
[SLIME](https://common-lisp.net/project/slime/) with Lisps running in Docker
containers. It can launch a container from an image, start a Lisp, and then
connect to it using SLIME.

To get started, describe the Lisp implementations and Docker images you want to
use in the variable `slime-docker-implementations`. Then, run `M-- M-x slime-docker`
and away you go.

It is highly recommended that you enable the `slime-tramp` contrib. If that is
enabled, this package will be able to let you use `M-.` and friends to visit
files that are both locally on your machine and inside the container.

This package defaults to using
[`daewok/lisp-devel:latest`](https://hub.docker.com/r/daewok/lisp-devel/) as the
Docker image. It contains SBCL, ABCL, CCL, and ECL along with Quicklisp and the
external libraries necessary to compile most of the packages in Quicklisp.

## Why not use SLIME directly? ##

It is definitely possible to manually start a container with a Lisp running
inside, start the Swank server in the Lisp process, and then connect from Emacs
using `M-x slime-connect`. But, if you're running the Docker container on the
same machine as Emacs, wouldn't it be nice to have SLIME start the container and
perform all the necessary set up? That's where this package comes in.

SLIME's initialization routine (for starting the inferior Lisp process) is
unfortunately not very flexible. It requires that both Lisp and Emacs have
access to the same filesystem (so the port Swank is listening on can be shared)
and that the port Swank listens on is the same port to which SLIME has to
connect. Neither of these are necessarily true with Docker.

This package watches the stdout of the Lisp process to figure out when Swank is
ready to accept connections. It also queries the Docker daemon to determine
which port 4005 has been forwarded to. Additionally, it automates the
integration with docker-machine (very nice for Windows and OSX users) and
provides access to many of the options to `docker run` (e.g. setting environment
variables, mounting folders, and so on).

## Quickstart ##

These instructions will get you up and running using SBCL on a Linux machine
without docker-machine.

  1. Install slime-docker. I recommend using
     [MELPA](http://melpa.org/#/getting-started) if you haven't already installed
     slime-docker manually.

  2. Add the following to your Emacs config:

     ```elisp
     ;; Do some standard SLIME configuration.
     (slime-setup '(slime-fancy slime-tramp))
     ;; Set the default lisp you want to use (here it's SBCL).
     (setq inferior-lisp-program "sbcl")
     ```
  3. Run `M-x slime-docker`


You have to do a little more work to use this with docker-machine, as it
requires some environment variables to be set before the `docker` command will
work. These instructions will get you up and running on any OS with
docker-machine.

  1. Install slime-docker. I recommend using
     [MELPA](http://melpa.org/#/getting-started) if you haven't already installed
     slime-docker manually.

  2. Make sure `docker-machine` is on your PATH. OSX users may find it useful to
     use [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)
     for this.

  3. Make sure the virtual machine that you want to use is running.

  4. Add the following to your Emacs config:

     ```elisp
     ;; Do some standard SLIME configuration.
     (slime-setup '(slime-fancy slime-tramp))
     ;; We'll use sbcl here, but we need to tell slime-docker that we're using
     ;; docker-machine. Change "default" to the name of the machine you're using.
     (setq slime-docker-implementations `((sbcl ("sbcl") :docker-machine "default")))
     ```
  5. Run `M-x slime-docker`

## Documentation ##

### Variables ###

+ `SLIME-DOCKER-IMPLEMENTATIONS`

A list of known Lisp implementations running on Docker.  The list should have
the form: `((NAME (PROGRAM PROGRAM-ARGS ...) &key KEYWORD-ARGS) ...)`

`NAME` is a symbol for the implementation.

`PROGRAM` and `PROGRAM-ARGS` are strings used to start the Lisp process inside
the Docker container.

For `KEYWORD-ARGS` see `SLIME-DOCKER-START`.

+ `SLIME-DOCKER-DEFAULT-LISP`

The name of the default Lisp implementation for `SLIME-DOCKER`.  See
`SLIME-DOCKER-IMPLEMENTATIONS`.

### Functions ###

+ `(slime-docker &optional command)`

Launch a Lisp process in a Docker container and connect SLIME to it.

The normal entry point to slime-docker.el. Similar to `SLIME` function. Tries to
guess the correct Lisp to start based on prefix arguments and the values of
`SLIME-DOCKER-IMPLEMENTATIONS` and `SLIME-DOCKER-DEFAULT-LISP`.

`COMMAND` is the command to run in the Docker container.

+ `(slime-docker-start &key (program inferior-lisp-program) program-args directory name (buffer "*docker-lisp*") (image-name "daewok/lisp-devel") (image-tag "latest") (rm t) env (init 'slime-docker--init-command) mounts coding-system (slime-mount-path "/usr/local/share/common-lisp/source/slime/") (slime-mount-read-only t) uid docker-machine (docker-command "docker") (docker-machine-setenv t) security-opts`

Start a Docker container and Lisp process in the container then connect to it.

Use when `SLIME-DOCKER` is not sufficient. Keyword arguments are also used in
`SLIME-DOCKER-IMPLEMENTATIONS`.

If the slime-tramp contrib is also loaded (highly recommended), this will also
set up the appropriate tramp translations to view and edit files in the spawned
container.

`PROGRAM` and `PROGRAM-ARGS` are the filename and argument strings for the Lisp
process.

`IMAGE-NAME` is a string naming the image that should be used to start the
container.

`IMAGE-TAG` is a string nameing the tag to use. Defaults to "latest".

`INIT` is a function that should return a string to load and start Swank. The
function will be called with no arguments - but that may change in a future
version.

`CODING-SYSTEM` is ignored.

`ENV` an alist of environment variables to set in the docker container.

`BUFFER` the name of the buffer to use for the subprocess.

`NAME` a symbol to describe the Lisp implementation.

`DIRECTORY` set this as the working directory in the container.

`RM` if true, the container is removed when the process closes.

`MOUNTS` a list describing the voluments to mount into the container. It is of
the form: `(((HOST-PATH . CONTAINER-PATH) &key READ-ONLY) ... )`

`UID` if specified, sets the UID of the Lisp process in the container.

`SLIME-MOUNT-PATH` the location where to mount SLIME into the container defaults
to "/usr/local/share/common-lisp/source/slime/"

`SLIME-MOUNT-READ-ONLY` if non-NIL, SLIME is mounted into the container as
read-only. Defaults to T.

`DOCKER-MACHINE` if non-NIL, must be a string naming a machine name known to
docker-machine. If provided, used to set appropriate environment variables for
the docker process to communicate with the desired machine. Does not start the
machine if it is currently not running.

`DOCKER-COMMAND` is the command to use when interacting with docker. Defaults to
"docker". See `SLIME-DOCKER-MACHINE-SSH-AGENT-HELPER-PATH` if you are using
docker-machine and would like to share your SSH Agent with the container.

`DOCKER-MACHINE-SETENV` if non-NIL, uses `setenv` to set Emacs environment with
the necessary variables from docker-machine. Should be non-NIL if you expect
tramp to work with images running in docker machine.

`SECURITY-OPTS` specifies --security-opt options when running 'docker run'. Must
be an alist where keys and values are strings. See README for note on using this
with SBCL.

