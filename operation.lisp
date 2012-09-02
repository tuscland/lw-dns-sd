(in-package #:zeroconf)

(defparameter *default-operation-next-result-timeout* nil) ;; wait indefinitely

(defclass operation ()
  ((handle
    :initarg :handle
    :reader operation-handle)
   (results-queue
    :initform (mp:make-mailbox))))

(defmethod initialize-instance :after ((self operation) &key handle)
  (assert (not (fli:null-pointer-p handle))))

(defmethod operation-next-result ((self operation)
                                  &key (timeout *default-operation-next-result-timeout*))
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
