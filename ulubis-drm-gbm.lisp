;;;; ulubis-drm-gbm.lisp

(in-package #:ulubis-drm-gbm)

(setf ulubis-backend:backend-name (intern "backend-drm-gbm" :ulubis-backend))

(defclass ulubis-backend:backend ()
  ((window :accessor window :initarg :window :initform nil)
   (counter :accessor counter :initarg :counter :initform 0)
   (mouse-button-handler :accessor mouse-button-handler :initarg :mouse-button-handler :initform (lambda (button state x y)))
   (mouse-motion-handler :accessor mouse-motion-handler :initarg :mouse-motion-handler :initform (lambda (x y xrel yrel state)))
   (keyboard-handler :accessor keyboard-handler :initarg :keyboard-handler :initform (lambda (key state)))
   (window-event-handler :accessor window-event-handler :initarg :window-event-handler :initform (lambda ()))
   (libinput-context :accessor libinput-context :initarg :libinput-context :initform nil)
   (libinput-fd :accessor libinput-fd :initarg :libinput-fd :initform nil)
   (tty-fd :accessor tty-fd :initarg :tty-fd :initform nil)))

(defconstant +KDSETMODE+ #x4B3A)
(defconstant +KD-TEXT+ #x00)
(defconstant +KD-GRAPHICS+ #x01)

(defparameter *keyboard-mode* nil)

(defconstant +KDGKBMODE+ #x4B44)
(defconstant +KDSKBMODE+ #x4B45)
(defconstant +K-OFF+ #x04)
(defconstant +KDSKBMUTE+ #x4B51)

(defmethod ulubis-backend:initialise-backend ((backend backend) width height devices)
  #+sbcl
  (sb-ext:disable-debugger)
  (setf (tty-fd backend) (nix:open "/dev/tty" (logior nix:o-rdwr nix:o-noctty)))

  ;; Stop input from leaking to tty
  (handler-case (nix:ioctl (tty-fd backend) +KDSKBMUTE+ 1)
    (nix:enotty ()
      (with-foreign-object (kb-mode :int)
	(nix:ioctl (tty-fd backend) +KDGKBMODE+ kb-mode)
	(setf *keyboard-mode* (mem-aref kb-mode :int)))
      (nix:ioctl (tty-fd backend) +KDSKBMODE+ +K-OFF+)))
  (nix:ioctl (tty-fd backend) +KDSETMODE+ +KD-GRAPHICS+)
  
  (cepl:repl width height 3.3)
  (gl:viewport 0 0 width height)
  (gl:disable :cull-face)
  (gl:disable :depth-test)
  (apply #'initialise-libinput backend devices))

(defun initialise-libinput (backend &rest devices)
  (with-slots (libinput-context libinput-fd) backend
    (setf libinput-context (libinput:path-create-context
			    (libinput:make-libinput-interface)
			    (null-pointer)))
    (setf libinput-fd (libinput:get-fd libinput-context))
    (if devices
	(mapcar (lambda (path)
		  (libinput:path-add-device libinput-context path))
		devices)
	(let ((device-paths (directory "/dev/input/event*")))
	  (loop :for device-path :in device-paths
	     :do
	     (let ((device (libinput:path-add-device libinput-context (namestring device-path))))
	       (when (not (cffi:null-pointer-p device))
		 (if (not (or (libinput:device-has-capability device libinput:device-cap-keyboard)
			      (libinput:device-has-capability device libinput:device-cap-pointer)))
		     (libinput:path-remove-device device)
		     (format t "Added device ~A~%" device-path)))))))))
	     

(defmacro with-event-handlers ((context event type) &body handlers)
  `(let ((,event (libinput:get-event ,context)))
     (loop :while (not (null-pointer-p ,event))
	:do (let ((,type (libinput:event-get-type ,event)))
		(cond
		  ,@(mapcar (lambda (handler)
			      `((= ,type ,(first handler)) ,(second handler)))
			    handlers))
		(libinput:event-destroy ,event)
		(setf ,event (libinput:get-event ,context))))))

#|
(defmethod process-events ((backend backend))
  (with-slots (libinput-context libinput-fd) backend
    (nix:with-pollfds (pollfds
		       (libinput-pollfd libinput-fd nix:pollin nix:pollpri))
      (when (nix:poll pollfds 1 16)
	(libinput:dispatch libinput-context)
	(with-event-handlers (libinput-context event type)
	  (libinput:keyboard-key (libinput:with-keyboard-key (event time key state)
				   (funcall (keyboard-handler backend)
					    time key state)))
	  (libinput:pointer-motion (libinput:with-pointer-motion (event time dx dy)
				     (funcall (mouse-motion-handler backend)
					      time dx dy)))
	  (libinput:pointer-button (libinput:with-pointer-button (event time button state)
				     (funcall (mouse-button-handler backend)
					      time button state))))))))
|#

(defmethod ulubis-backend:process-events ((backend backend))
  (with-slots (libinput-context) backend
    (libinput:dispatch libinput-context)
    (with-event-handlers (libinput-context event type)
      (libinput:keyboard-key (libinput:with-keyboard-key (event time key state)
			       (funcall (keyboard-handler backend)
					time key state)))
      (libinput:pointer-motion (libinput:with-pointer-motion (event time dx dy)
				 (funcall (mouse-motion-handler backend)
					  time dx dy)))
      (libinput:pointer-button (libinput:with-pointer-button (event time button state)
				 (funcall (mouse-button-handler backend)
					  time button state))))))

(defmethod ulubis-backend:get-fd ((backend backend))
  (libinput-fd backend))

;; Bother with these methods or just setf?
(defmethod ulubis-backend:register-keyboard-handler ((backend backend) keyboard-handler)
  (setf (keyboard-handler backend) keyboard-handler))

(defmethod ulubis-backend:register-mouse-motion-handler ((backend backend) mouse-motion-handler)
  (setf (mouse-motion-handler backend) mouse-motion-handler))

(defmethod ulubis-backend:register-mouse-button-handler ((backend backend) mouse-button-handler)
  (setf (mouse-button-handler backend) mouse-button-handler))

(defmethod ulubis-backend:register-window-event-handler ((backend backend) window-event-handler)
  (setf (window-event-handler backend) window-event-handler))

(defmethod ulubis-backend:swap-buffers ((backend backend))
  (cepl:swap))

(defmethod ulubis-backend:destroy-backend ((backend backend))
  (cepl:quit)
  (if *keyboard-mode*
      (nix:ioctl (tty-fd backend) +KDSKBMODE+ *keyboard-mode*)
      (nix:ioctl (tty-fd backend) +KDSKBMUTE+ 0))
  (nix:ioctl (tty-fd backend) +KDSETMODE+ +KD-TEXT+)
  (nix:close (tty-fd backend)))
