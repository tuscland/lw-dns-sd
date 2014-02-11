;;;; -*- encoding: utf-8; mode: LISP; syntax: COMMON-LISP; indent-tabs-mode: nil -*-

;;; DNS Service Discovery for LispWorks.
;;; Copyright (c) 2013, Camille Troillard. All rights reserved.

;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing,
;;; software distributed under the License is distributed on an "AS
;;; IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
;;; express or implied.  See the License for the specific language
;;; governing permissions and limitations under the License.

;;; Base errors definitions.


(in-package "COM.WILDORA.DNS-SD")

(define-condition dns-sd-error (error)
  ()
  (:documentation "All errors specific to DNS-SD are of this type."))

(define-condition timeout-error (dns-sd-error)
  ())

(define-condition library-not-available-error (dns-sd-error)
  ())

(define-condition result-error (dns-sd-error)
  ((code
    :reader result-error-code
    :initarg :code))
  (:report (lambda (condition stream)
             (format stream
                     "~A (~X)"
                     (or (error-code-description
                          (result-error-code condition))
                         "Unknown Error")
                     (result-error-code condition))))
  (:documentation "Signalled if a DNS-SD foreign function returns an error code."))

(defconstant +error-code-descriptions+
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

(defun error-code-description (code)
  (cdr (assoc code +error-code-descriptions+)))

(defun check-error-code (code)
  (unless (zerop code)
    (error 'result-error :code code))
  code)
