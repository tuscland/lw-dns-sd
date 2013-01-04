(in-package #:com.wildora.dnssd)

(defclass record-operation (operation)
  ())

(defmethod record-add ((service-operation service-operation) (record record))
  )

(defmethod record-update ((service-operation service-operation)
                          (record-operation record-operation)
                          (record record))
  )

(defmethod record-remove ((service-operation service-operation)
                          (record-operation record-operation))
  (dns-service-remove-record
   (operation-handle service-operation)
   (operation-handle record-operation)
   0)
  (setf (slot-value record-operation 'handle) nil))
