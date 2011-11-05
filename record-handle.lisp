(in-package #:zeroconf)

(defclass record-handle (handle)
  ())

(defmethod record-add ((service-handle service-handle) rrtype data ttl)
  )

(defmethod record-update ((service-handle service-handle)
                          (record-handle record-handle)
                          rrtype data ttl)
  )

(defmethod record-remove ((service-handle service-handle)
                          (record-handle record-handle))
  (dns-service-remove-record
   (handle-ref service-handle)
   (handle-ref record-handle)
   0)
  (setf (slot-value record-handle 'ref) nil))

