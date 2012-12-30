(defpackage com.wildora.dnssd.operation
  (:import-from #:com.wildora.dnssd.result
   #:result
   #:result-more-coming-p
   #:make-result
   #:make-operation-error-result)
  (:import-from #:com.wildora.dnssd.conditions
   #:error-code-p
   #:dnssd-error)
  (:import-from #:com.wildora.dnssd.foreign
   #:dns-service-get-property
   #:dns-service-sockfd
   #:dns-service-deallocate
   #:dns-service-process-result))

(in-package #:com.wildora.dnssd.operation)


(defun daemon-version ()
  (fli:with-dynamic-foreign-objects ((result :uint32)
                                     (size :uint32
                                           :initial-element (fli:size-of :uint32)))
    (dns-service-get-property "DaemonVersion" result size)
    (fli:dereference result)))


(declaim (hcl:special-dynamic *current-operation*))

(defun current-operation ()
  (assert (not (null *current-operation*)))
  *current-operation*)

(defmacro with-current-operation (operation &body body)
  `(let ((*current-operation* ,operation))
     ,@body))


(require "comm")

(define-condition operation-timeout-error (dnssd-error)
  ()
  (:default-initargs
   :format-control "Waiting for operation result timed out."))


(defclass operation (comm:socket-stream)
  ((handle
    :accessor operation-handle
    :initform (error "HANDLE must be specified")
    :initarg :handle)
   (cancel-after-reply-p
    :reader %cancel-after-reply-p
    :initform nil
    :initarg :cancel-after-reply)
   (service-prototype
    :reader operation-service-prototype
    :initform nil
    :initarg :service-prototype)
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
  (if (operation-cancelled-p self)
      (warn "Operation ~A has already been canceled" self)
    (progn
      (dns-service-deallocate (operation-handle self))
      (setf (operation-handle self) nil))))

(defmethod operation-cancelled-p ((self operation))
  (fli:null-pointer-p
   (operation-handle self)))

(defmethod operation-enqueue-result ((self operation) (result result))
  (mp:mailbox-send
   (operation-results-queue self)
   result))

(defparameter *default-result-timeout* nil) ;; wait indefinitely

(defmethod operation-next-result ((self operation)
                                  &key (timeout *default-result-timeout*))
  (multiple-value-bind (object flag)
      (mp:mailbox-read (operation-results-queue self)
                       "Waiting for next operation result"
                       timeout)
    (if flag
        object
      (error 'operation-timeout-error))))

(defmethod operation-collect-results ((self operation)
                                      &key (timeout *default-result-timeout*)
                                           (test 'result-more-coming-p))
  (loop :for result := (operation-next-result self :timeout timeout)
        :collect result
        :while (funcall test result)))

(defmethod operation-wait-result ((self operation)
                                  &key (test (constantly t))
                                       (timeout *default-result-timeout*))
  (loop :for result := (operation-next-result self :timeout timeout)
        :when (funcall test result)
        :do (return result)))

(defmethod operation-invoke-callback ((self operation) (result result))
  (funcall (or (operation-callback self)
               'operation-enqueue-result) self result))

(defmethod operation-reply ((self operation) error-code more-coming-p &rest result-properties)
  (if (error-code-p error-code)
      (dnssd-error error-code)
    (operation-invoke-callback self
                               (make-result more-coming-p
                                            result-properties))))

(defmethod %process-operation ((self operation))
  (with-current-operation self
    (dns-service-process-result (operation-handle self)))
  (%cancel-after-reply-p self))

(defmethod process-operation ((self operation))
  "Called from the dispatched to process pending results."
  (handler-case (%process-operation self)
    (dnssd-error (condition)
      (operation-invoke-callback self
                                 (make-operation-error-result condition))
      ;; return t to indicate that the operation is no longer valid
      t)))
