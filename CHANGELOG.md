## Unreleased

### Added

- Add support for providing network to Docker run.
- Add ports option.
- Add dns option.

### Removed

- Removed slime-docker-sbcl-seccomp-profile variable and bundled seccomp
  profiles. SBCL stopped trying to disable ASLR a while ago.

### Changed

- Change default image to clfoundation/cl-devel
- Use cl-prefix for cl-lib functions.
- Check if file exists before running MAKE-DIRECTORY.
- Fix TRAMP-MAKE-TRAMP-FILE-NAME for Emacs 26.
- Expand file names before mounting them.
