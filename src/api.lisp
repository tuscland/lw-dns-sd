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

;;; Main public API.


(in-package "COM.WILDORA.DNS-SD")

(declaim (special-dynamic *results-mailbox*))

(defmacro bind-results-mailbox ((mailbox) &body body)
  `(let ((*results-mailbox* ,mailbox))
     ,@body))

(defun results-mailbox ()
  *results-mailbox*)

(defun call-def-operation (service-fn)
  (unless (boundp '*results-mailbox*)
    (error "Use BIND-RESULTS-MAILBOX prior to run an operation"))
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (funcall service-fn pointer)
    (dispatch :handle (fli:dereference pointer)
              :mailbox *results-mailbox*)))

(defmacro def-operation (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (call-def-operation (lambda (pointer) ,@body))))

(def-operation register (port type
                         &key (interface-index +interface-index-any+)
                              name domain host txt-record no-auto-rename)
  (check-type port (integer 0))
  (check-type type string)
  (check-type interface-index (integer 0))
  (when name
    (check-type name string)
    (assert (< (length name) +max-service-name-length+)))
  (when domain
    (check-type domain string)
    (assert (< (length domain) +max-domain-name-length+)))
  (when host
    (check-type host string))
  (when (and no-auto-rename (null name))
    (error "NO-AUTO-RENAME cannot be specified in conjunction with the default (nil) service NAME."))
  ;; see section 6.1 of http://files.dns-sd.org/draft-cheshire-dnsext-dns-sd.txt
  (assert (< (length txt-record) 8900))
  (unless txt-record
    (setf txt-record (build-txt-record nil)))
  (let ((flags (if no-auto-rename
                   +flag-no-auto-rename+
                 +flag-no-flag+)))
    (with-static-array-pointer (txt-pointer txt-record)
      (dns-service-register
       pointer
       flags
       interface-index
       name
       type
       domain
       host
       (htons port)
       (length txt-record)
       txt-pointer
       (make-callback-pointer 'dns-service-register-reply)
       nil))))

(def-operation enumerate-domains (&key (interface-index +interface-index-any+) (domains :browse-domains))
  (check-type interface-index (integer 0))
  (assert (member domains '(:browse-domains :registration-domains)))
  (let ((flags (cdr (assoc domains *enumerated-domains-flags*))))
    (dns-service-enumerate-domains
     pointer
     flags
     interface-index
     (make-callback-pointer 'dns-service-enumerate-domains-reply)
     nil)))

(def-operation browse (type &key (interface-index +interface-index-any+) domain) 
  (check-type type string)
  (check-type interface-index (integer 0))
  (when domain
    (check-type domain string))
  (dns-service-browse
   pointer
   0
   interface-index
   type
   domain
   (make-callback-pointer 'dns-service-browse-reply)
   nil))

(def-operation resolve (name type domain
                        &key (interface-index +interface-index-any+) broadcasting)
  (check-type name string)
  (check-type type string)
  (check-type domain string)
  (check-type interface-index (integer 0))
  (when broadcasting
    (assert (eq broadcasting :force-multicast)))
  (dns-service-resolve
   pointer
   (broadcasting-option-to-flag broadcasting)
   interface-index
   name
   type
   domain
   (make-callback-pointer 'dns-service-resolve-reply)
   nil))

(def-operation get-addr-info (hostname &key (interface-index +interface-index-any+) protocols broadcasting)
  (check-type hostname string)
  (check-type interface-index (integer 0))
  (when protocols
    (loop :for protocol :in protocols
          :do (assert (member protocol '(:ipv4 :ipv6)))))
  (when broadcasting
    (assert (member broadcasting
                    '(:force-multicast :long-lived-query))))
  (dns-service-get-addr-info
   pointer
   (broadcasting-option-to-flag broadcasting)
   interface-index
   (protocols-to-flags protocols)
   hostname
   (make-callback-pointer 'dns-service-get-addr-info-reply)
   nil))

(def-operation query-record (full-name rrtype
                             &key (interface-index +interface-index-any+) (rrclass :IN) broadcasting)
  (check-type full-name string)
  (check-type interface-index (integer 0))
  (unless interface-index
    (setf interface-index +interface-index-any+))
  (when broadcasting
    (assert (member broadcasting '(:force-multicast :long-lived-query))))
  (dns-service-query-record
   pointer
   (broadcasting-option-to-flag broadcasting)
   interface-index
   full-name
   (keyword-to-rrtype rrtype)
   (keyword-to-rrclass rrclass)
   (make-callback-pointer 'dns-service-query-record-reply)
   nil))

(def-operation nat-port-mapping-create (internal-port
                                        &key (interface-index +interface-index-any+)
                                             (external-port 0) protocols (ttl 0))
  (check-type protocols list)
  (check-type internal-port (integer 0))
  (check-type external-port (integer 0))
  (check-type ttl (integer 0))
  (check-type interface-index (integer 0))
  (dns-service-nat-port-mapping-create
   pointer
   0
   interface-index
   (protocols-to-flags protocols)
   internal-port
   external-port
   ttl
   (make-callback-pointer 'dns-service-nat-port-mapping-create-reply)
   nil))

(def-operation create-connection ()
  (dns-service-create-connection pointer))


;;;
;;; Record based routines
;;;

(defun register-record (operation full-name rrtype rdata
                        &key (interface-index +interface-index-any+)
                             (rrclass :IN) identity (ttl 0))
  (check-type operation operation)
  (assert (member identity '(:shared :unique)))
  (check-type full-name string)
  (check-type rdata (array (unsigned-byte 8)))
  (assert (< (length rdata) 65536))
  (check-type ttl integer)
  (check-type interface-index (integer 0))
  (let ((flags (or (when (eq identity :shared) +flag-shared+)
                   (when (eq identity :unique) +flag-unique+))))
    (with-static-array-pointer (rdata-pointer rdata)
      (fli:with-dynamic-foreign-objects ((pointer record-ref))
        (dns-service-register-record
         (operation-handle operation)
         pointer
         flags
         interface-index
         full-name
         (keyword-to-rrtype rrtype)
         (keyword-to-rrclass rrclass)
         (length rdata)
         rdata-pointer
         ttl
         (make-callback-pointer 'dns-service-register-record-reply)
         nil)
        (fli:dereference pointer)))))

(defun add-record (operation rrtype rdata &key (ttl 0))
  (check-type operation operation)
  (check-type rdata (array (unsigned-byte 8)))
  (assert (< (length rdata) 65536))
  (check-type ttl integer)
  (with-static-array-pointer (rdata-pointer rdata)
    (fli:with-dynamic-foreign-objects ((pointer record-ref))
      (dns-service-add-record
       (operation-handle operation)
       pointer
       0
       (keyword-to-rrtype rrtype)
       (length rdata)
       rdata-pointer
       ttl)
      (fli:dereference pointer))))

(defun update-record (operation record-ref rdata &key (ttl 0))
  (check-type operation operation)
  (check-type rdata (array (unsigned-byte 8)))
  (assert (< (length rdata) 65536))
  (check-type ttl integer)
  (with-static-array-pointer (rdata-pointer rdata)
    (dns-service-update-record
     (operation-handle operation)
     record-ref
     0
     (length rdata)
     rdata-pointer
     ttl)))

(defun remove-record (operation record-ref)
  (check-type operation operation)
  (dns-service-remove-record (operation-handle operation) record-ref 0)
  (setf (fli:pointer-address record-ref) 0))

(defun reconfirm-record (full-name rrtype rdata
                         &key (interface-index +interface-index-any+) (rrclass :IN) force)
  (check-type interface-index (integer 0))
  (check-type full-name string)
  (check-type rdata (array (unsigned-byte 8)))
  (assert (< (length rdata) 65536))
  (let ((flags (if force +flag-force+ +flag-no-flag+)))
    (with-static-array-pointer (rdata-pointer rdata)
      (dns-service-reconfirm-record
       flags
       interface-index
       full-name
       (keyword-to-rrtype rrtype)
       (keyword-to-rrclass rrclass)
       (length rdata)
       rdata-pointer))))
