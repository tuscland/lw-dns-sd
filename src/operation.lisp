(defpackage #:com.wildora.dnssd.operation
  (:import-from #:com.wildora.dnssd.event
   #:event
   #:check-event
   #:event-more-coming-p
   #:make-event
   #:make-operation-error-event)
  (:import-from #:com.wildora.dnssd.conditions
   #:error-code-p
   #:dnssd-error)
  (:import-from #:com.wildora.dnssd.foreign
   #:dns-service-get-property
   #:dns-service-sockfd
   #:dns-service-deallocate
   #:dns-service-process-result))

(in-package #:com.wildora.dnssd.operation)


(defparameter *default-event-timeout* 60)

(defun daemon-version ()
  (fli:with-dynamic-foreign-objects ((result :uint32)
                                     (size :uint32
                                           :initial-element (fli:size-of :uint32)))
    (dns-service-get-property "DaemonVersion" result size)
    (fli:dereference result)))


(declaim (special-dynamic *current-operation*))

(defun current-operation ()
  (assert (not (null *current-operation*)))
  *current-operation*)

(defmacro with-current-operation (operation &body body)
  `(let ((*current-operation* ,operation))
     ,@body))


(define-condition event-timeout-error (dnssd-error)
  ()
  (:default-initargs
   :format-control "Waiting for operation event timed out"))


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
   (events-queue
    :reader operation-events-queue
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

(defmethod operation-enqueue-event ((self operation) (event event))
  (mp:mailbox-send
   (operation-events-queue self)
   event))

(defmethod operation-next-event ((self operation)
                                  &key (timeout *default-event-timeout*))
  (check-event
   (multiple-value-bind (object flag)
       (mp:mailbox-read (operation-events-queue self)
                        "Waiting for next operation event"
                        timeout)
     (if flag
         object
       (error 'event-timeout-error)))))

(defmethod operation-collect-events ((self operation)
                                      &key (timeout *default-event-timeout*)
                                           (test #'event-more-coming-p))
  (loop :for event := (operation-next-event self :timeout timeout)
        :collect event
        :while (funcall test event)))

(defmethod operation-wait-event ((self operation)
                                  &key (test (constantly t))
                                       (timeout *default-event-timeout*))
  (loop :for event := (operation-next-event self :timeout timeout)
        :when (funcall test event)
        :do (return event)))

(defmethod operation-invoke-callback ((self operation) (event event))
  (funcall (or (operation-callback self)
               #'operation-enqueue-event) self event))

(defmethod operation-reply ((self operation) error-code more-coming-p &rest event-values)
  (if (error-code-p error-code)
      (dnssd-error error-code)
    (operation-invoke-callback self
                               (make-event more-coming-p
                                           event-values))))

(defmethod %process-operation ((self operation))
  (with-current-operation self
    (dns-service-process-result (operation-handle self)))
  (%cancel-after-reply-p self))

(defmethod process-operation ((self operation))
  "Called from the dispatched to process pending events."
  (handler-case (%process-operation self)
    (dnssd-error (condition)
      (operation-invoke-callback self
                                 (make-operation-error-event condition))
      ;; return t to indicate that the operation is no longer valid
      t)))
