(in-package #:zeroconf)

(defstruct service
  (interface-index 0)
  name
  full-name
  type
  domain
  host
  (port 0)
  txt-record)

(defstruct domain
  (interface-index 0)
  name
  defaultp)

(defclass responder ()
  ((callback-function
    :accessor responder-callback-function
    :initarg :callback-function
    :initform #'debug-print)
   (error-function
    :accessor responder-error-function
    :initarg :error-function
    :initform #'debug-print)))
