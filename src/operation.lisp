(in-package #:zeroconf)

(defparameter *default-operation-timeout* nil) ;; wait indefinitely

(defclass operation ()
  ((handle
    :initarg :handle
    :reader operation-handle)
   (results-queue
    :initform (mp:make-mailbox))))

(defgeneric cancel (operation))

(defmethod initialize-instance :after ((self operation) &key handle)
  (assert (not (fli:null-pointer-p handle))))

(defmethod operation-next-result ((self operation)
                                  &key (timeout *default-operation-timeout*))
  (mp:mailbox-read
   (slot-value self 'results-queue)
   "Waiting for next operation result"
   timeout))

(defmethod operation-enqueue-result ((self operation) result)
  (mp:mailbox-send
   (slot-value self 'results-queue)
   result))

(defmethod operation-cancelled-p ((self operation))
  (fli:null-pointer-p (operation-handle self)))

(defmethod operation-collect-results ((self operation)
                                      &key (timeout *default-operation-timeout*)
                                           (predicate 'result-more-coming-p))
  (loop :for result := (operation-next-result self :timeout timeout)
        :while (funcall predicate result)
        :collect result))

(defmacro with-operation ((operation operation-form) &body body)
  `(let (,operation)
     (unwind-protect
         (progn
           (setf ,operation ,operation-form)
           ,@body)
       (cancel ,operation))))
