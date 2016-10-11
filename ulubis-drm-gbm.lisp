;;;; ulubis-drm-gbm.lisp

(in-package #:ulubis-backend)

(defparameter backend-name 'backend-drm-gbm)

(defclass backend-drm-gbm ()
  ((window :accessor window :initarg :window :initform nil)
   (counter :accessor counter :initarg :counter :initform 0)
   (mouse-button-handler :accessor mouse-button-handler :initarg :mouse-button-handler :initform (lambda (button state x y)))
   (mouse-motion-handler :accessor mouse-motion-handler :initarg :mouse-motion-handler :initform (lambda (x y xrel yrel state)))
   (keyboard-handler :accessor keyboard-handler :initarg :keyboard-handler :initform (lambda (key state)))
   (window-event-handler :accessor window-event-handler :initarg :window-event-handler :initform (lambda ()))
   (libinput-context :accessor libinput-context :initarg :libinput-context :initform nil)
   (libinput-fd :accessor libinput-fd :initarg :libinput-fd :initform nil)))

(defmethod initialise-backend ((backend backend-drm-gbm) width height)
  (sb-ext:disable-debugger)
  (cepl:repl width height 3.3)
  (gl:viewport 0 0 width height)
  (gl:disable :cull-face)
  (gl:disable :depth-test)
  (initialise-libinput backend
		       "/dev/input/event5"
		       "/dev/input/event8"))

(defun initialise-libinput (backend &rest devices)
  (with-slots (libinput-context libinput-fd) backend
    (setf libinput-context (libinput:path-create-context
			    (libinput:make-libinput-interface)
			    (null-pointer)))
    (setf libinput-fd (libinput:get-fd libinput-context))
    (mapcar (lambda (path)
	      (libinput:path-add-device libinput-context path))
	    devices)))

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

(defmethod process-events ((backend backend-drm-gbm))
  (with-slots (libinput-context libinput-fd) backend
    (when (sb-unix:unix-simple-poll libinput-fd :input 16)
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
					    time button state)))))))

;; Bother with these methods or just setf?
(defmethod register-keyboard-handler ((backend backend-drm-gbm) keyboard-handler)
  (setf (keyboard-handler backend) keyboard-handler))

(defmethod register-mouse-motion-handler ((backend backend-drm-gbm) mouse-motion-handler)
  (setf (mouse-motion-handler backend) mouse-motion-handler))

(defmethod register-mouse-button-handler ((backend backend-drm-gbm) mouse-button-handler)
  (setf (mouse-button-handler backend) mouse-button-handler))

(defmethod register-window-event-handler ((backend backend-drm-gbm) window-event-handler)
  (setf (window-event-handler backend) window-event-handler))

(defmethod swap-buffers ((backend backend-drm-gbm))
  (cepl:swap))

(defmethod destroy-backend ((backend backend-drm-gbm))
  (cepl:quit))
