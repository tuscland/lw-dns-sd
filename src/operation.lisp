(in-package #:com.wildora.dnssd)

(defparameter *default-result-timeout* 60)

(defun daemon-version ()
  (fli:with-dynamic-foreign-objects ((result :uint32)
                                     (size :uint32
                                           :initial-element (fli:size-of :uint32)))
    (dns-service-get-property "DaemonVersion" result size)
    (fli:dereference result)))


(define-condition result-timeout-error (dnssd-error)
  ())

(defclass operation (comm:socket-stream)
  ((handle
    :accessor operation-handle
    :initform (error "HANDLE must be specified")
    :initarg :handle)
   (callback
    :reader operation-callback
    :initform nil
    :initarg :callback)
   (results-queue
    :reader operation-results-queue
    :initform (mp:make-mailbox)))
  (:default-initargs
   :direction :input))

(defmethod initialize-instance :after ((self operation) &key handle)
  (setf (comm:socket-stream-socket self)
        (dns-service-sockfd handle)))

(defmethod cancel-operation ((self operation))
  (if (operation-canceled-p self)
      (warn "Operation ~A has already been canceled" self)
    (progn
      (dns-service-deallocate (operation-handle self))
      (setf (fli:pointer-address
             (operation-handle self)) 0))))

(defmethod operation-canceled-p ((self operation))
  (fli:null-pointer-p
   (operation-handle self)))

(defmethod operation-enqueue-result ((self operation) (result result))
  (mp:mailbox-send
   (operation-results-queue self)
   result))

(defmethod operation-next-result ((self operation)
                                  &key (timeout *default-result-timeout*))
  (check-result
   (multiple-value-bind (object no-timeout-p)
       (mp:mailbox-read (operation-results-queue self)
                        "Waiting for next operation result"
                        timeout)
     (if no-timeout-p
         object
       (error 'result-timeout-error)))))

(defmethod operation-invoke-callback ((self operation) (result result))
  (funcall (or (operation-callback self)
               #'operation-enqueue-result)
           self result))


(declaim (special-dynamic *current-operation*))

(defmethod reply (error-code more-coming-p &rest result-values)
  (maybe-signal-result-error error-code)
  (assert *current-operation*)
  (operation-invoke-callback *current-operation*
                             (make-result more-coming-p result-values)))

(defmethod process-result ((self operation))
  "Called from the dispatched to process pending results."
  (handler-bind ((result-error
                  #'(lambda (condition)
                      (operation-invoke-callback
                       self (make-error-result condition)))))
    (let ((*current-operation* self))
      (dns-service-process-result
       (operation-handle self)))))
