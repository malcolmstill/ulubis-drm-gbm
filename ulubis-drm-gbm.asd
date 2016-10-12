;;;; ulubis-drm-gbm.asd

(asdf:defsystem #:ulubis-drm-gbm
  :description "DRM/GBM backend for the Ulubis Wayland compositor"
  :author "Malcolm Still"
  :license "BSD 3-Clause"
  :depends-on (#:cffi
               #:cepl.drm-gbm
	       #:cl-libinput)
  :serial t
  :components ((:file "ulubis-drm-gbm")))

