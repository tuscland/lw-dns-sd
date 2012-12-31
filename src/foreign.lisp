(defpackage com.wildora.dnssd.foreign
  (:import-from #:com.wildora.dnssd.conditions
   #:error-code-p
   #:dnssd-error))

(in-package #:com.wildora.dnssd.foreign)


(fli:define-foreign-type service-ref ()
  '(:pointer :void))

(fli:define-foreign-type record-ref ()
  '(:pointer :void))

(fli:define-foreign-type dnssd-string ()
  `(:ef-mb-string
    :null-terminated-p t
    :external-format :utf-8))

(fli:define-c-typedef (error-t
                       (:foreign-name "DNSServiceErrorType"))
  :int32)

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
  (sa_data (:pointer :char)))

#+win32
(fli:define-c-struct (sockaddr
                      (:foreign-name "sockaddr"))
  (sa_family :uint16)
  (sa_data (:pointer :char)))

#+lispworks6.1
(defun ip-address-from-sockaddr (pointer)
  (comm:ip-address-string
   (ecase (fli:foreign-slot-value pointer 'sa_family)
     (#.comm::*socket_af_inet*
      (fli:with-coerced-pointer
          (addr-in :type '(:pointer (:struct comm::sockaddr_in))) pointer
        (comm::sockaddr-in-to-lisp addr-in)))
     (#.comm::*socket_af_inet6*
      (fli:with-coerced-pointer
          (addr-in :type '(:pointer (:struct comm::sockaddr_in6))) pointer
        (comm::sockaddr-in6-to-lisp addr-in))))))

#+lispworks6.0
(defun ip-address-from-sockaddr (pointer)
  (comm:ip-address-string
   (ecase (fli:foreign-slot-value pointer 'sa_family)
     (#.comm::*socket_af_inet*
      (fli:with-coerced-pointer
          (addr-in :type '(:pointer (:struct comm::sockaddr_in))) pointer
        (fli:foreign-slot-value addr-in '(comm::sin_addr comm::s_addr)))))))

(fli:define-foreign-function (dns-service-deallocate
                              "DNSServiceRefDeallocate" :source)
    ((sdref service-ref))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function (dns-service-sockfd
                              "DNSServiceRefSockFD" :source)
    ((sdref service-ref))
  :result-type :int
  :language :ansi-c)

(defmacro define-dnssd-function ((name external-name) args)
  "Declares a foreign function that returns a value of type
DNSServiceErrorType and defines a wrapper around the foreign function
that raises an error of type DNSSD-RESULT-ERROR if the foreign
function returns a value indicating that an error occurred."
  (let ((unwrapped-name (intern (format nil "%~A" name)))
	(result (gensym "RESULT"))
        (arg-names (mapcar #'car args)))
    `(dspec:def (define-dnssd-function (,name ,external-name))
       (fli:define-foreign-function (,unwrapped-name ,external-name :source)
           ,args
         :result-type error-t
         :language :ansi-c)
       (defun ,name ,arg-names
         (let ((,result (,unwrapped-name ,@arg-names)))
           (if (error-code-p ,result)
               (dnssd-error ,result)
             ,result))))))

(define-dnssd-function (dns-service-get-property
                        "DNSServiceGetProperty")
  ((property (:reference-pass dnssd-string))
   (result (:pointer :void))
   (size (:pointer :uint32))))

(define-dnssd-function (dns-service-process-result
                        "DNSServiceProcessResult")
  ((sdref service-ref)))

(define-dnssd-function (dns-service-add-record
                        "DNSServiceAddRecord")
  ((sdref service-ref)
   (rdref (:pointer record-ref))
   (flags flags-t)
   (rrtype :uint16)
   (rdlen :uint16)
   (data (:const (:pointer :void)))
   (ttl :uint32)))

(define-dnssd-function (dns-service-update-record
                        "DNSServiceUpdateRecord")
  ((sdref service-ref)
   (rdref record-ref)
   (flags flags-t)
   (rdlen :uint16)
   (data (:const (:pointer :void)))
   (ttl :uint32)))

(define-dnssd-function (dns-service-remove-record
                        "DNSServiceRemoveRecord")
  ((sdref service-ref)
   (rdref record-ref)
   (flags flags-t)))


;;;;
;;;; Low level versions of operations functions
;;;;

(define-dnssd-function (%dns-service-register
                        "DNSServiceRegister")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (name (:reference-pass dnssd-string :allow-null t))
   (type (:reference-pass dnssd-string))
   (domain (:reference-pass dnssd-string :allow-null t))
   (host (:reference-pass dnssd-string :allow-null t))
   (port :uint16)
   (txtlen :uint16)
   (txtrecord (:const (:pointer :void)))
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dnssd-function (%dns-service-enumerate-domains
                        "DNSServiceEnumerateDomains")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dnssd-function (%dns-service-browse
                        "DNSServiceBrowse")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (type (:reference-pass dnssd-string))
   (domain (:reference-pass dnssd-string :allow-null t))
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dnssd-function (%dns-service-resolve
                        "DNSServiceResolve")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (name (:reference-pass dnssd-string))
   (type (:reference-pass dnssd-string))
   (domain (:reference-pass dnssd-string :allow-null t))
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dnssd-function (%dns-service-get-addr-info
                        "DNSServiceGetAddrInfo")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (protocol protocol-t)
   (hostname (:reference-pass dnssd-string))
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dnssd-function (%dns-service-query-record
                        "DNSServiceQueryRecord")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (full-name (:reference-pass dnssd-string))
   (rrtype :uint16)
   (rrclass :uint16)
   (callback (:pointer :function))
   (context (:pointer :void))))

(define-dnssd-function (%dns-service-nat-port-mapping-create
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