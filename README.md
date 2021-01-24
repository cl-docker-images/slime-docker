[![MELPA](http://melpa.org/packages/slime-docker-badge.svg)](http://melpa.org/#/slime-docker) [![MELPA Stable](http://stable.melpa.org/packages/slime-docker-badge.svg)](http://stable.melpa.org/#/slime-docker)
# slime-docker.el #

This emacs package is designed to easily integrate
[SLIME](https://common-lisp.net/project/slime/) with Lisps running in Docker
containers. It can launch a container from an image, start a Lisp, and then
connect to it using SLIME.

If you already have SLIME installed and your favorite implementation specified
in `inferior-lisp-program`, all you should need to do is run `M-x
slime-docker`. Otherwise, see the Quickstart.

## Quickstart ##

On most machines nowadays, all you should need to do in order to use this is
install Docker, set it up so that you can run docker without using sudo, and
then install this package. Assuming you're using `use-package`, the following
should be sufficient:

```elisp
(use-package slime-docker
  :custom
  (slime-docker-program "sbcl"))
```

Then run `M-x slime-docker`

This will set you up to use sbcl inside a Docker container. Feel free to change
the default implementation or perform other customizations.

If you are using Docker machine, you likely have to add the following
customization as well:

```elisp
(slime-docker-docker-machine "default")
```

This package has not been tested on machines that require super user access to
connect to the Docker socket. However, it may be possible if you write a script
that handles all the authentication and use that script for
`slime-docker-docker-command`.

## Default Image ##

This package defaults to using
[`clfoundation/cl-devel:latest`](https://hub.docker.com/r/clfoundation/cl-devel/)
as the Docker image. It contains SBCL, ABCL, CCL, and ECL along with Quicklisp
and the external libraries necessary to compile most of the packages in
Quicklisp.

If you will be using the default image for serious development, it is
recommended that you mount a folder on top of `/home/cl` such that you can
persist configuration changes and fasl caches between containers. Below is the
recommended configuration for this:

```elisp
(use-package slime-docker
  :custom
  (slime-docker-program "sbcl")
  (slime-docker-mounts `(((,(expand-file-name "~/cl-devel/") . "/home/cl/")))))
```

One hiccupp is that the image comes with some folders in `/home/cl` to provide
the default config. If you want to use this default config as a starting point
for your own, simply run the `unpack-default-home-dir` command in the
container. You can do this from Lisp using:

```common-lisp
(require :asdf)
(uiop:run-program '("unpack-default-home-dir"))
```

You may want to restart your Lisp after that in order to take advantage of the
newly written config.

## Multiple Configurations ##

If you would like to define a list of potential configurations to use, set them
in `slime-docker-implementations`. You can then choose from these
implementations using `M-- M-x slime-docker`.

Patches to put a sane defcustom interface on top of
`slime-docker-implementations` are welcome!

## Other Packages ##

It is highly recommended that you enable the `slime-tramp` contrib. If that is
enabled, this package will be able to let you use `M-.` and friends to visit
files that are both locally on your machine and inside the container.

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
