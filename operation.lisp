(in-package #:zeroconf)

(defvar *service-reply-operation* nil)

(defmacro bind-operation (operation &body body)
  `(let ((*service-reply-operation* ,operation))
     ,@body))

(defmacro with-bound-operation (operation &body body)
  `(let ((,operation *service-reply-operation*))
     ,@body))


(defclass operation (comm:socket-stream)
  ((handle
    :type dns-service-ref
    :initarg :handle
    :reader operation-handle)
   (responder
    :initform (error "An operation cannot be instanciated without a responder.")
    :reader operation-responder
    :initarg :responder)
   (user-info
    :initform nil
    :reader operation-user-info
    :initarg :user-info)
   (cancel-after-reply-p
    :initarg :cancel-after-reply
    :reader operation-cancel-after-reply-p
    :initform nil))
  (:default-initargs
   :direction :io))

(defmethod shared-initialize :after ((self operation) slot-names &key)
  (setf (comm:socket-stream-socket self)
        (dns-service-sockfd
         (operation-handle self))))

(defmethod %cancel ((self operation))
  (dns-service-deallocate
   (operation-handle self))
  (setf (slot-value self 'handle) nil))

(defmethod operation-invoke-callback ((self operation) error-code &rest args)
  (if (= error-code +no-err+)
      (apply
       (responder-callback-function
        (operation-responder self)) args)
    (dns-sd-error error-code)))

(defmethod %operation-process-result ((self operation))
  (bind-operation self
     (dns-service-process-result
      (operation-handle self)))
  (operation-cancel-after-reply-p self))

(defmethod operation-process-result ((self operation))
  (handler-case (%operation-process-result self)
    (dns-sd-error (condition)
      (funcall
       (responder-error-function
        (operation-responder self)) condition)
      t)))
