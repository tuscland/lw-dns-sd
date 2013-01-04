(in-package #:com.wildora.dnssd)

(define-condition dnssd-error (error)
  ()
  (:documentation "All errors specific to DNSSD are of this type."))

(define-condition result-error (dnssd-error)
  ((code
    :reader result-error-code
    :initarg :code))
  (:report (lambda (condition stream)
             (format stream
                     "~A (~X)"
                     (result-code-description
                      (result-error-code condition))
                     (result-error-code condition))))
  (:documentation "Signaled if a DNSSD foreign function returns an error code."))

(defparameter *result-codes-descriptions*
  '((-65537 . "Unknown error")
    (-65538 . "No such name error")
    (-65539 . "No memory error")
    (-65540 . "Bad parameter error")
    (-65541 . "Bad reference error")
    (-65542 . "Bad state error")
    (-65543 . "Bad flags error")
    (-65544 . "Unsupported error")
    (-65545 . "Not initialized error")
    (-65547 . "Already registered error")
    (-65548 . "Name conflict error")
    (-65549 . "Invalid error")
    (-65551 . "Incompatible error")
    (-65552 . "Bad interface index error")))

(defun result-code-description (code)
  (cdr (assoc code *result-codes-descriptions*)))

(defun maybe-signal-result-error (code)
  (unless (zerop code)
    (error 'result-error :code code))
  code)
