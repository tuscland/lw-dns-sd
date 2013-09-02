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

;;; Low-level foreign language interface.


(in-package "COM.WILDORA.DNS-SD")

(fli:define-c-typedef service-ref
  :pointer)

(fli:define-c-typedef record-ref
  :pointer)

(fli:define-foreign-type dns-sd-string ()
  `(:ef-mb-string
    :null-terminated-p t
    :external-format :utf-8))

(fli:define-foreign-converter
    error-t ()
    result-code
  :foreign-type :int32
  :foreign-to-lisp `(maybe-signal-result-error ,result-code))

(fli:define-c-typedef (flags-t
                       (:foreign-name "DNSServiceFlags"))
  :uint32)

(fli:define-c-typedef (protocol-t
                       (:foreign-name "DNSServiceProtocol"))
  :uint32)

#+macosx
(fli:define-c-struct (sockaddr
                      (:foreign-name "sockaddr"))
  (sa_len :uint8)
  (sa_family :uint8)
  (sa_data (:c-array :uint8)))

#+(or mswindows linux)
(fli:define-c-struct (sockaddr
                      (:foreign-name "sockaddr"))
  (sa_family :uint16)
  (sa_data (:c-array :uint8)))

#-lispworks6.0
(defun ip-address-from-sockaddr (pointer)
  (ecase (fli:foreign-slot-value pointer 'sa_family)
    (#.comm::*socket_af_inet*
     (fli:with-coerced-pointer
         (addr-in :type '(:pointer (:struct comm::sockaddr_in))) pointer
       (values :ipv4 (comm:ip-address-string
                      (comm::sockaddr-in-to-lisp addr-in)))))
    (#.comm::*socket_af_inet6*
     (fli:with-coerced-pointer
         (addr-in :type '(:pointer (:struct comm::sockaddr_in6))) pointer
       (values :ipv6 (comm:ip-address-string
                      (comm::sockaddr-in6-to-lisp addr-in)))))))

#+lispworks6.0
(defun ip-address-from-sockaddr (pointer)
  (ecase (fli:foreign-slot-value pointer 'sa_family)
    (#.comm::*socket_af_inet*
     (fli:with-coerced-pointer
         (addr-in :type '(:pointer (:struct comm::sockaddr_in))) pointer
       (values :ipv4 (comm:ip-address-string
                      (fli:foreign-slot-value addr-in '(comm::sin_addr comm::s_addr))))))))

(defun check-external-module ()
  #-macosx
  (handler-case
      (fli:register-module "dnssd"
                           :connection-style :immediate)
    (error ()
      (error 'library-not-available-error))))

(fli:define-foreign-function (dns-service-deallocate
                              "DNSServiceRefDeallocate")
    ((sdref service-ref))
  :result-type :void)

(fli:define-foreign-function (dns-service-sockfd
                              "DNSServiceRefSockFD")
    ((sdref service-ref))
  :result-type :int)

(defmacro define-dns-sd-function ((name external-name) args)
  "Declares a foreign function that returns a value of type
DNSServiceErrorType and defines a wrapper around the foreign function
that raises an error of type RESULT-ERROR if the foreign function
returns a value indicating that an error occurred."
  (let ((unwrapped-name (intern (string-append '% name)))
        (arg-names (mapcar #'car args)))
    `(progn
       (fli:define-foreign-function (,unwrapped-name ,external-name)
           ,args
         :result-type error-t)
       (defun ,name ,arg-names
         (check-external-module)
         (,unwrapped-name ,@arg-names)))))

(define-dns-sd-function (dns-service-get-property
                         "DNSServiceGetProperty")
  ((property (:reference-pass dns-sd-string))
   (result (:pointer :void))
   (size (:pointer :uint32))))

(define-dns-sd-function (dns-service-process-result
                         "DNSServiceProcessResult")
  ((sdref service-ref)))

(define-dns-sd-function (dns-service-construct-full-name
                         "DNSServiceConstructFullName")
  ((full-name (:pointer :char))
   (service (:reference-pass dns-sd-string :allow-null t))
   (type (:reference-pass dns-sd-string))
   (domain (:reference-pass dns-sd-string))))

;;;;
;;;; Low level versions of operations functions
;;;;

(define-dns-sd-function (%dns-service-register
                         "DNSServiceRegister")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (name (:reference-pass dns-sd-string :allow-null t))
   (type (:reference-pass dns-sd-string))
   (domain (:reference-pass dns-sd-string :allow-null t))
   (host (:reference-pass dns-sd-string :allow-null t))
   (port :uint16)
   (txtlen :uint16)
   (txtrecord (:const (:pointer :void)))
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dns-sd-function (%dns-service-enumerate-domains
                         "DNSServiceEnumerateDomains")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dns-sd-function (%dns-service-browse
                         "DNSServiceBrowse")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (type (:reference-pass dns-sd-string))
   (domain (:reference-pass dns-sd-string :allow-null t))
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dns-sd-function (%dns-service-resolve
                         "DNSServiceResolve")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (name (:reference-pass dns-sd-string))
   (type (:reference-pass dns-sd-string))
   (domain (:reference-pass dns-sd-string :allow-null t))
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dns-sd-function (%dns-service-get-addr-info
                         "DNSServiceGetAddrInfo")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (protocol protocol-t)
   (hostname (:reference-pass dns-sd-string))
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dns-sd-function (%dns-service-query-record
                         "DNSServiceQueryRecord")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (full-name (:reference-pass dns-sd-string))
   (rrtype :uint16)
   (rrclass :uint16)
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dns-sd-function (%dns-service-nat-port-mapping-create
                         "DNSServiceNATPortMappingCreate")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (protocol protocol-t)
   (internal-port :uint16)
   (external-port :uint16)
   (ttl :uint32)
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dns-sd-function (%dns-service-register-record
                         "DNSServiceRegisterRecord")
  ((sdref service-ref)
   (rdref (:pointer record-ref))
   (flags flags-t)
   (interface-index :uint32)
   (full-name (:reference-pass dns-sd-string))
   (rrtype :uint16)
   (rrclass :uint16)
   (rdlen :uint16)
   (rdata (:pointer :void))
   (ttl :uint32)
   (callback (:pointer :function))
   (context (:pointer :void))))


(define-dns-sd-function (dns-service-create-connection
                         "DNSServiceCreateConnection")
  ((sdref (:pointer service-ref))))

(define-dns-sd-function (%dns-service-add-record
                         "DNSServiceAddRecord")
  ((sdref service-ref)
   (rdref (:pointer record-ref))
   (flags flags-t)
   (rrtype :uint16)
   (rdlen :uint16)
   (rdata (:pointer :void))
   (ttl :uint32)))

(define-dns-sd-function (%dns-service-update-record
                         "DNSServiceUpdateRecord")
  ((sdref service-ref)
   (rdref record-ref)
   (flags flags-t)
   (rdlen :uint16)
   (data (:const (:pointer :void)))
   (ttl :uint32)))

(define-dns-sd-function (%dns-service-remove-record
                         "DNSServiceRemoveRecord")
  ((sdref service-ref)
   (rdref record-ref)
   (flags flags-t)))

(define-dns-sd-function (%dns-service-reconfirm-record
                         "DNSServiceReconfirmRecord")
  ((flags flags-t)
   (interface-index :uint32)
   (full-name (:reference-pass dns-sd-string))
   (type :uint16)
   (class :uint16)
   (rdlen :uint16)
   (rdata (:pointer :void))))
