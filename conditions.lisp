(in-package #:zeroconf)

(defstruct error-info
  class
  code
  description)

(defparameter *dns-sd-errors* (make-hash-table :test #'eql)
  "A table of DNS SD API error codes, corresponding Lisp error
classes, and text descriptions of the errors.")

(defmacro def-dns-sd-error (class code description)
  "Defines a DNS SD error based on a DNS SD API error code."
  `(eval-when (:load-toplevel :execute)
     (dspec:def (def-dns-sd-error ,class)
       (setf (gethash ,code *dns-sd-errors*)
	     (make-error-info :class ',class :code ,code :description ,description))
       (define-condition ,class (dns-sd-result-error)
	 ()))))

(defun make-dns-sd-error (code)
  (let ((info (gethash code *dns-sd-errors*)))
    (if info
	(make-condition (error-info-class info) :code code)
      ;; We couldn't find any info, which is weird, but at least we
      ;; can instanciate a condition.
      (make-condition 'dns-sd-result-error :code code))))

(defun dns-sd-error (code)
  "Given a DNS SD API error code, raises the corresponding Lisp error.
If the code is unknown, an error of type DNS-SD-RESULT-ERROR is
raised."
  (error
   (make-dns-sd-error code)))

(defun error-code->error-class (code)
  (let ((info (gethash code *dns-sd-errors*)))
    (error-info-class info)))
  

(define-condition dns-sd-error (simple-error)
  ()
  (:documentation "All errors specific to DNS SD are of this type."))

(define-condition dns-sd-result-error (dns-sd-error)
  ((code :initarg :code :initform nil :accessor dns-service-error-code))
  (:report
   (lambda (condition stream)
     (let* ((code (dns-service-error-code condition))
	    (info (gethash code *dns-sd-errors*)))
       (if info
	   (format stream "The following DNS SD error occurred: ~A (code ~A)"
		   (error-info-description info)
		   code)
	   (format stream "An unknown DNS SD error occurred, with code ~A." code)))))
  (:documentation "Signaled if a foreign DNS SD API function returns an error code."))


;; Totally stealing this idiom of setting an implicitly created
;; function's documentation slot from Edi Weitz.

(setf (documentation 'dns-service-error-code 'function)
      "Returns the lowlevel DNS SD API error code associated with an error.")

;; Now let's define some error classes.


(def-dns-sd-error dns-sd-unknown-error
                  -65537
                  "Unknown error (is the DNS Service Discovery system daemon running?)")

(def-dns-sd-error dns-sd-no-such-name-error
                  -65538
                  "No such name error")

(def-dns-sd-error dns-sd-no-memory-error
                  -65539
                  "No memory error")

(def-dns-sd-error dns-sd-bad-param-error
                  -65540
                  "Bad parameter error")

(def-dns-sd-error dns-sd-bad-reference-error
                  -65541
                  "Bad reference error")

(def-dns-sd-error dns-sd-bad-state-error
                  -65542
                  "Bad state error")

(def-dns-sd-error dns-sd-bad-flags-error
                  -65543
                  "Bad flags error")

(def-dns-sd-error dns-sd-unsupported-error
                  -65544
                  "Unsupported error")

(def-dns-sd-error dns-sd-not-initialized-error
                  -65545
                  "Not initialized error")

(def-dns-sd-error dns-sd-already-registered-error
                  -65547
                  "Already registered error")

(def-dns-sd-error dns-sd-name-conflict-error
                  -65548
                  "Name conflict error")

(def-dns-sd-error dns-sd-invalid-error
                  -65549
                  "Invalid error")

(def-dns-sd-error dns-sd-incompatible-error
                  -65551
                  "Incompatible error")

(def-dns-sd-error dns-sd-bad-interface-index-error
                  -65552
                  "Bad interface index error")


(define-condition service-already-published-error (dns-sd-error)
  ((service :initarg :service :initform nil :accessor service-already-published-error-service))
  (:report
   (lambda (condition stream)
     (format stream "The service ~S is already being published."
	     (service-already-published-error-service condition)))))

(define-condition service-not-published-error (dns-sd-error)
  ((service :initarg :service :initform nil :accessor service-not-published-error-service))
  (:report
   (lambda (condition stream)
     (format stream "The service ~S has not been published."
	     (service-not-published-error-service condition)))))

(define-condition socket-fd-error (dns-sd-error)
  ((oid :initarg :oid :initform nil :accessor socket-fd-error-oid))
  (:report
   (lambda (condition stream)
     (format stream "DNS-SD OID ~S has no socket file descriptor." (socket-fd-error-oid condition)))))
