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

(defclass responder ()
  ((callback-function
    :accessor responder-callback-function
    :initarg :callback-function
    :initform #'lw:do-nothing)
   (error-function
    :accessor responder-error-function
    :initarg :error-function
    :initform #'lw:do-nothing)))
