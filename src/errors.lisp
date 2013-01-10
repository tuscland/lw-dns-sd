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
  '((-65538 . "No such name")
    (-65539 . "No memory")
    (-65540 . "Bad parameter")
    (-65541 . "Bad reference")
    (-65542 . "Bad state")
    (-65543 . "Bad flags")
    (-65544 . "Unsupported")
    (-65545 . "Not initialized")
    (-65547 . "Already registered")
    (-65548 . "Name conflict")
    (-65549 . "Invalid")
    (-65550 . "Firewall")
    (-65551 . "Client library incompatible with daemon")
    (-65552 . "Bad interface index")
    (-65553 . "Refused")
    (-65554 . "No such record")
    (-65555 . "No auth")
    (-65556 . "No such key")
    (-65557 . "NAT Traversal")
    (-65558 . "Double NAT")
    (-65559 . "Bad time")
    (-65560 . "Bad signature")
    (-65561 . "Bad key")
    (-65562 . "Transient")
    (-65563 . "Background daemon not running")
    (-65564 . "NAT port mapping unsupported (NAT doesn't support NAT-PMP or UPnP)")
    (-65565 . "NAT port mapping disabled (NAT supports NAT-PMP or UPnP but it's disabled by the administrator)")
    (-65566 . "No router currently configured (probably no network connectivity)")
    (-65567 . "Polling mode")
    (-65568 . "Timeout")))

(defun result-code-description (code)
  (or (cdr (assoc code *result-codes-descriptions*))
      "Unknown"))

(defun maybe-signal-result-error (code)
  (unless (zerop code)
    (error 'result-error :code code))
  code)
