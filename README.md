[![MELPA](http://melpa.org/packages/slime-docker-badge.svg)](http://melpa.org/#/slime-docker) [![MELPA Stable](http://stable.melpa.org/packages/slime-docker-badge.svg)](http://stable.melpa.org/#/slime-docker)
# slime-docker.el #

This emacs package is designed to easily integrate SLIME with Lisps running in
Docker containers. It can launch a container from an image, start a Lisp, and
then connect to it using SLIME.

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

SLIME's initialization routine is not very flexible. It requires that both Lisp
and Emacs have access to the same filesystem (so the port Swank is listening on
can be shared) and that the port Swank listens on is the same port to which
SLIME has to connect. Neither of these are necessarily true with Docker.

This package watches the stdout of the Lisp process to figure out when Swank is
ready to accept connections. It also queries the Docker daemon to determine
which port 4005 has been forwarded to.

## Some gotchas ##

### SBCL ASLR ###

SBCL does its best to turn off ASLR while it is starting. However, the default
Docker seccomp profile disallows this, resulting in the following message being
printed every time SBCL starts:

> WARNING:
> Couldn't re-execute SBCL with proper personality flags (/proc isn't mounted? setuid?)
> Trying to continue anyway.

If you have seccomp support compiled in Docker and would like to get rid of this
message, you have at least these two options:

+ Disable seccomp for the SBCL container, by adding `("seccomp" . "unconfined")`
  to the `:security-opts` list.

+ Use the modified seccomp profile provided by this package to enable full use
  of the `personality` syscall. To do this, add `("seccomp" . slime-docker-sbcl-seccomp-profile)`
  to the `:security-opts` list.
