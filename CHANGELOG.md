## Unreleased

## v0.8.2 - January 24, 2021

### Removed

- Removed coding-system arg.

## v0.8.1 - January 24, 2021

### Changed

- Fixed some package-lint issues and byte compilation warnings.

## v0.8 - January 24, 2021

### Added

- Add support for providing network to Docker run.
- Add ports option.
- Add dns option.
- Support for SLIME installed via straight.el.
- If uid argument is T (default), the container is started with the UID of the
  current user.
- Add gid argument with same behavior as uid argument.
- Add `defcustom`s for most arguments.

### Removed

- Removed slime-docker-sbcl-seccomp-profile variable and bundled seccomp
  profiles. SBCL stopped trying to disable ASLR a while ago.

### Changed

- Change default image to clfoundation/cl-devel
- Use cl-prefix for cl-lib functions.
- Check if file exists before running MAKE-DIRECTORY.
- Fix TRAMP-MAKE-TRAMP-FILE-NAME for Emacs 26.
- Expand file names before mounting them.
- Emacs no longer hangs if the image needs to be pulled.
