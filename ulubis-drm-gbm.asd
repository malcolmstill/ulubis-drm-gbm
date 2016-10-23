;;;; ulubis-drm-gbm.asd

(asdf:defsystem #:ulubis-drm-gbm
  :description "DRM/GBM backend for the Ulubis Wayland compositor"
  :author "Malcolm Still"
  :license "BSD 3-Clause"
  :depends-on (#:cffi
	       #:osicat
               #:cepl.drm-gbm
	       #:cl-libinput
	       #:ulubis)
  :serial t
  :components ((:file "package")
	       (:file "ulubis-drm-gbm")))

