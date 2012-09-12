(in-package #:zeroconf)

(declaim (hcl:special-dynamic *fli-reply-service-operation*))

(defmacro bind-service-operation (operation &body body)
  `(let ((*fli-reply-service-operation* ,operation))
     ,@body))

(defmacro with-bound-service-operation (operation &body body)
  `(let ((,operation *fli-reply-service-operation*))
     ,@body))


(defclass service-operation (operation comm:socket-stream)
  ((callback
    :reader service-operation-callback
    :initform nil
    :initarg :callback)
   (service-prototype
    :initform nil
    :reader service-operation-service-prototype
    :initarg :service-prototype)
   (cancel-after-reply-p
    :initform nil
    :reader service-operation-cancel-after-reply-p
    :initarg :cancel-after-reply))
  (:default-initargs
   :direction :input))

(defmethod initialize-instance :after ((self service-operation) &key handle)
  (setf (comm:socket-stream-socket self)
        (dns-service-sockfd handle)))

(defmethod %cancel ((self service-operation))
  (if (operation-cancelled-p self)
      (warn "Operation has already been cancelled")
    (progn
      (dns-service-deallocate (operation-handle self))
      (setf (slot-value self 'handle) nil))))

(defmethod %service-operation-process-result ((self service-operation))
  (bind-service-operation self
    (dns-service-process-result (operation-handle self)))
  (service-operation-cancel-after-reply-p self))

(defmethod %service-operation-invoke-callback ((self service-operation) result)
  (let ((callback (service-operation-callback self)))
    (if callback
        (funcall callback self result)
      (operation-enqueue-result self result))))

(defmethod service-operation-invoke-callback ((self service-operation) error-code result)
  (if (= error-code +no-err+)
      (%service-operation-invoke-callback self result)
    (zeroconf-error error-code)))

(defmethod service-operation-process-result ((self service-operation))
  (handler-case (%service-operation-process-result self)
    (zeroconf-error (condition)
      (%service-operation-invoke-callback self (make-error-result :condition condition))
      t)))
