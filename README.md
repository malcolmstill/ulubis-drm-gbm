
# ulubis-drm-gbm

ulubis-drm-gbm is a backend for [the ulubis window compositor](https://github.com/malcolmstill/ulubis). It allows ulubis to run without X.

# Requirements

ulubis-drm-gbm requires: cffi, osicat, cepl.drm-gbm, cl-libinput.

# Installation

After installing ulubis with `(ql:quickload :ulubis)`, scripts in the `build` directory will automatically download `ulubis-drm-gbm`. Otherwise you can run `(ql:quickload :ulubis-drm-gbm)` directly.
