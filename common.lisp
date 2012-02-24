(in-package #:zeroconf)


(defstruct service
  (interface-index 0)
  name
  full-name
  type
  domain
  host
  (port 0)
  properties)


(defun service-eq (service1 service2)
  "Compares two services.  Two services are 'eq when they have the same name."
  (and (service-p service1)
       (service-p service2)
       (string-equal (service-name service1)
                     (service-name service2))))

(defstruct record
  (interface-index 0)
  name
  type
  class
  data
  ttl)


(defstruct domain
  (interface-index 0)
  name
  defaultp)


(defclass handle ()
  ((ref
    :initarg :ref
    :reader handle-ref)))
