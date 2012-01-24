(in-package #:zeroconf)

(defvar *fli-reply-service-handle* nil)

(defmacro bind-service-handle (service-handle &body body)
  `(let ((*fli-reply-service-handle* ,service-handle))
     ,@body))

(defmacro with-bound-service-handle (service-handle &body body)
  `(let ((,service-handle *fli-reply-service-handle*))
     ,@body))


(defclass service-handle (handle comm:socket-stream)
  ((callback-function
    :initform (error "SERVICE-HANDLE cannot be instanciated without a CALLBACK-FUNCTION.")
    :initarg :callback-function
    :reader service-handle-callback-function)
   (error-function
    :initform #'lw:do-nothing
    :initarg :error-function
    :reader service-handle-error-function)
   (user-info
    :initform nil
    :reader service-handle-user-info
    :initarg :user-info)
   (cancel-after-reply-p
    :initarg :cancel-after-reply
    :reader service-handle-cancel-after-reply-p
    :initform nil))
  (:default-initargs
   :direction :input))

(defmethod initialize-instance :after ((self service-handle) &key ref)
  (assert (not (fli:null-pointer-p ref)))
  (setf (comm:socket-stream-socket self)
        (dns-service-sockfd ref)))

(defmethod %cancel ((self service-handle))
  (if (fli:null-pointer-p (handle-ref self))
      (warn "Service handle has already been cancelled.")
    (progn 
      (dns-service-deallocate (handle-ref self))
      (setf (slot-value self 'ref) nil))))

(defmethod %service-handle-process-result ((self service-handle))
  (bind-service-handle self
     (dns-service-process-result (handle-ref self)))
  (service-handle-cancel-after-reply-p self))

(defmethod service-handle-invoke-callback ((self service-handle) error-code &rest args)
  (if (= error-code +no-err+)
      (apply (service-handle-callback-function self)
             self args)
    (zeroconf-error error-code)))

(defmethod service-handle-process-result ((self service-handle))
  (handler-case (%service-handle-process-result self)
    (zeroconf-error (condition)
      (funcall (service-handle-error-function self)
               self condition)
      t)))
