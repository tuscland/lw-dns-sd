(in-package #:zeroconf)

(defstruct error-info
  class
  code
  description)


(define-condition zeroconf-error (error)
  ()
  (:documentation "All errors specific to Zeroconf are of this type."))

(define-condition zeroconf-result-error (zeroconf-error
                                         infra:system-error)
  ()
  (:documentation "Signaled if a foreign Zeroconf API function returns an error code."))


(defmacro define-zeroconf-error (class code description)
  "Defines a Zeroconf error based on a Zeroconf API error code."
  `(dspec:def (define-zeroconf-error ,class)
     (infra:define-system-error zeroconf-result-error ,class ,code ,description)))

(editor:setup-indent 'define-zeroconf-error 1)

(defun raise-zeroconf-error (code)
  "Given a Zeroconf API error code, raises the corresponding Lisp error.
If the code is unknown, an error of type DNS-SD-RESULT-ERROR is
raised."
  (infra:raise-system-error 'zeroconf-result-error code))


(define-zeroconf-error unknown-error
  -65537
  "Unknown error (is the DNS Service Discovery system daemon running?)")

(define-zeroconf-error no-such-name-error
  -65538
  "No such name error")

(define-zeroconf-error no-memory-error
  -65539
  "No memory error")

(define-zeroconf-error bad-param-error
  -65540
  "Bad parameter error")

(define-zeroconf-error bad-reference-error
  -65541
  "Bad reference error")

(define-zeroconf-error bad-state-error
  -65542
  "Bad state error")

(define-zeroconf-error bad-flags-error
  -65543
  "Bad flags error")

(define-zeroconf-error unsupported-error
  -65544
  "Unsupported error")

(define-zeroconf-error not-initialized-error
  -65545
  "Not initialized error")

(define-zeroconf-error already-registered-error
  -65547
  "Already registered error")

(define-zeroconf-error name-conflict-error
  -65548
  "Name conflict error")

(define-zeroconf-error invalid-error
  -65549
  "Invalid error")

(define-zeroconf-error incompatible-error
  -65551
  "Incompatible error")

(define-zeroconf-error bad-interface-index-error
  -65552
  "Bad interface index error")


(define-condition service-already-published-error (zeroconf-error)
  ((service
    :initarg :service
    :initform nil
    :accessor service-already-published-error-service))
  (:report
   (lambda (condition stream)
     (format stream "The service ~S is already being published."
	     (service-already-published-error-service condition)))))

(define-condition service-not-published-error (zeroconf-error)
  ((service
    :initarg :service
    :initform nil
    :accessor service-not-published-error-service))
  (:report
   (lambda (condition stream)
     (format stream "The service ~S has not been published."
	     (service-not-published-error-service condition)))))
