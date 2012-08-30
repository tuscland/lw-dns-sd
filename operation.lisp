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

(defstruct error-result
  condition)

(defstruct resolve-result
  more-coming-p
  service)

(defstruct register-result
  conflict-p
  service)

(defstruct enumerate-domains-result
  more-coming-p
  presence
  domain)

(defstruct browse-result
  more-coming-p
  presence
  service)

(defstruct get-addr-info-result
  more-coming-p
  invalid-p
  interface-index
  hostname
  address
  ttl)

(defstruct query-record-result
  more-coming-p
  presence
  record)
