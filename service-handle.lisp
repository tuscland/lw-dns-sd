(in-package #:zeroconf)

(defvar *fli-reply-service-handle* nil)

(defmacro bind-service-handle (service-handle &body body)
  `(let ((*fli-reply-service-handle* ,service-handle))
     ,@body))

(defmacro with-bound-service-handle (service-handle &body body)
  `(let ((,service-handle *fli-reply-service-handle*))
     ,@body))


(defclass service-handle (handle comm:socket-stream)
  ((responder
    :initform (error "SERVICE-HANDLE cannot be instanciated without a RESPONDER.")
    :reader service-handle-responder
    :initarg :responder)
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

(defmethod initialize-instance :after ((self service-handle) &key)
  (setf (comm:socket-stream-socket self)
        (dns-service-sockfd (handle-ref self))))

(defmethod %cancel ((self service-handle))
  (dns-service-deallocate (handle-ref self))
  (setf (slot-value self 'ref) nil))

(defmethod %service-handle-process-result ((self service-handle))
  (bind-service-handle self
     (dns-service-process-result (handle-ref self)))
  (service-handle-cancel-after-reply-p self))

(defmethod service-handle-invoke-callback ((self service-handle) error-code &rest args)
  (if (= error-code +no-err+)
      (let ((responder (service-handle-responder self)))
        (apply (responder-callback-function responder)
               responder self args))
    (dns-sd-error error-code)))

(defmethod service-handle-process-result ((self service-handle))
  (handler-case (%service-handle-process-result self)
    (dns-sd-error (condition)
      (let ((responder (service-handle-responder self)))
        (funcall (responder-error-function responder)
                 responder self condition))
      t)))
