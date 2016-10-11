;;;; ulubis-drm-gbm.asd

(asdf:defsystem #:ulubis-drm-gbm
  :description "Describe ulubis-drm-gbm here"
  :author "Malcolm Still"
  :license "Specify license here"
  :depends-on (#:cffi
               #:cepl.drm-gbm
	       #:cl-libinput)
  :serial t
  :components ((:file "ulubis-drm-gbm")))

