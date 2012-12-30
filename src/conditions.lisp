(defpackage com.wildora.dnssd.conditions)
(in-package #:com.wildora.dnssd.conditions)


(defun error-code-p (code)
  (not (zerop code)))

(define-condition dnssd-error (simple-error)
  ()
  (:documentation "All errors specific to Dnssd are of this type."))

(define-condition dnssd-result-error (dnssd-error
                                      infra:system-error)
  ()
  (:documentation "Signaled if a foreign Dnssd API function returns an error code."))

(defmacro define-dnssd-error (class code description)
  "Defines a Dnssd error based on a Dnssd API error code."
  `(dspec:def (define-dnssd-error ,class)
     (infra:define-system-error dnssd-result-error ,class ,code ,description)))

(defun dnssd-error (code)
  "Given a Dnssd API error code, raises the corresponding Lisp error.
If the code is unknown, an error of type DNS-SD-RESULT-ERROR is
raised."
  (infra:system-error 'dnssd-result-error code))

(define-dnssd-error unknown-error
  -65537
  "Unknown error (is the DNS Service Discovery system daemon running?)")

(define-dnssd-error no-such-name-error
  -65538
  "No such name error")

(define-dnssd-error no-memory-error
  -65539
  "No memory error")

(define-dnssd-error bad-param-error
  -65540
  "Bad parameter error")

(define-dnssd-error bad-reference-error
  -65541
  "Bad reference error")

(define-dnssd-error bad-state-error
  -65542
  "Bad state error")

(define-dnssd-error bad-flags-error
  -65543
  "Bad flags error")

(define-dnssd-error unsupported-error
  -65544
  "Unsupported error")

(define-dnssd-error not-initialized-error
  -65545
  "Not initialized error")

(define-dnssd-error already-registered-error
  -65547
  "Already registered error")

(define-dnssd-error name-conflict-error
  -65548
  "Name conflict error")

(define-dnssd-error invalid-error
  -65549
  "Invalid error")

(define-dnssd-error incompatible-error
  -65551
  "Incompatible error")

(define-dnssd-error bad-interface-index-error
  -65552
  "Bad interface index error")


(define-condition service-already-published-error (dnssd-error)
  ((service
    :initarg :service
    :initform nil
    :accessor service-already-published-error-service))
  (:report
   (lambda (condition stream)
     (format stream "The service ~S is already being published."
	     (service-already-published-error-service condition)))))

(define-condition service-not-published-error (dnssd-error)
  ((service
    :initarg :service
    :initform nil
    :accessor service-not-published-error-service))
  (:report
   (lambda (condition stream)
     (format stream "The service ~S has not been published."
	     (service-not-published-error-service condition)))))
