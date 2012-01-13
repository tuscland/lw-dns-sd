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
