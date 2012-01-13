(in-package #:zeroconf)

(defstruct error-info
  class
  code
  description)


(define-condition dns-sd-error (error)
  ()
  (:documentation "All errors specific to DNS SD are of this type."))

(define-condition dns-sd-result-error (dns-sd-error
                                       infra:system-error)
  ()
  (:documentation "Signaled if a foreign DNS SD API function returns an error code."))


(defmacro define-dns-sd-error (class code description)
  "Defines a DNS SD error based on a DNS SD API error code."
  `(eval-when (:load-toplevel :execute)
     (infra:define-system-error dns-sd-result-error ,class ,code ,description)))

(editor:setup-indent 'define-dns-sd-error 1)

(defun raise-dns-sd-error (code)
  "Given a DNS SD API error code, raises the corresponding Lisp error.
If the code is unknown, an error of type DNS-SD-RESULT-ERROR is
raised."
  (infra:raise-system-error 'dns-sd-result-error code))


(define-dns-sd-error unknown-error
  -65537
  "Unknown error (is the DNS Service Discovery system daemon running?)")

(define-dns-sd-error no-such-name-error
  -65538
  "No such name error")

(define-dns-sd-error no-memory-error
  -65539
  "No memory error")

(define-dns-sd-error bad-param-error
  -65540
  "Bad parameter error")

(define-dns-sd-error bad-reference-error
  -65541
  "Bad reference error")

(define-dns-sd-error bad-state-error
  -65542
  "Bad state error")

(define-dns-sd-error bad-flags-error
  -65543
  "Bad flags error")

(define-dns-sd-error unsupported-error
  -65544
  "Unsupported error")

(define-dns-sd-error not-initialized-error
  -65545
  "Not initialized error")

(define-dns-sd-error already-registered-error
  -65547
  "Already registered error")

(define-dns-sd-error name-conflict-error
  -65548
  "Name conflict error")

(define-dns-sd-error invalid-error
  -65549
  "Invalid error")

(define-dns-sd-error incompatible-error
  -65551
  "Incompatible error")

(define-dns-sd-error bad-interface-index-error
  -65552
  "Bad interface index error")


(define-condition service-already-published-error (dns-sd-error)
  ((service
    :initarg :service
    :initform nil
    :accessor service-already-published-error-service))
  (:report
   (lambda (condition stream)
     (format stream "The service ~S is already being published."
	     (service-already-published-error-service condition)))))

(define-condition service-not-published-error (dns-sd-error)
  ((service
    :initarg :service
    :initform nil
    :accessor service-not-published-error-service))
  (:report
   (lambda (condition stream)
     (format stream "The service ~S has not been published."
	     (service-not-published-error-service condition)))))
