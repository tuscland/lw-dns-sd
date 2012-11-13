(in-package #:zeroconf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(defun fli-make-array-from-bytes (pointer length)
  (let ((array (make-array length
                           :element-type '(unsigned-byte 8))))
    (fli:replace-foreign-array array
                               pointer
                               :end2 length)))


#+lispworks6.1
(defun fli-sockaddr-to-string (pointer)
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
(defun fli-sockaddr-to-string (pointer)
  (comm:ip-address-string
   (ecase (fli:foreign-slot-value pointer 'sa_family)
     (#.comm::*socket_af_inet*
      (fli:with-coerced-pointer
          (addr-in :type '(:pointer (:struct comm::sockaddr_in))) pointer
        (fli:foreign-slot-value addr-in '(comm::sin_addr comm::s_addr)))))))

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

(defmacro define-zeroconf-function ((name external-name) args)
  "Declares a foreign function that returns a value of type
DNSServiceErrorType and defines a wrapper around the foreign function
that raises an error of type ZEROCONF-RESULT-ERROR if the foreign
function returns a value indicating that an error occurred."
  (let ((unwrapped-name (intern (format nil "%~A" name)))
	(result (gensym "RESULT"))
        (arg-names (mapcar #'car args)))
    `(dspec:def (define-zeroconf-function (,name ,external-name))
       (fli:define-foreign-function (,unwrapped-name ,external-name :source)
           ,args
         :result-type error-t
         :language :ansi-c)
       (defun ,name ,arg-names
         (let ((,result (,unwrapped-name ,@arg-names)))
           (if (= ,result +no-err+)
               ,result
             (zeroconf-error ,result)))))))

(define-zeroconf-function (dns-service-get-property
                           "DNSServiceGetProperty")
  ((property (:reference-pass dnssd-string))
   (result (:pointer :void))
   (size (:pointer :uint32))))

(define-zeroconf-function (dns-service-process-result
                           "DNSServiceProcessResult")
  ((sdref service-ref)))

(define-zeroconf-function (dns-service-add-record
                           "DNSServiceAddRecord")
  ((sdref service-ref)
   (rdref (:pointer record-ref))
   (flags flags-t)
   (rrtype :uint16)
   (rdlen :uint16)
   (data (:const (:pointer :void)))
   (ttl :uint32)))

(define-zeroconf-function (dns-service-update-record
                           "DNSServiceUpdateRecord")
  ((sdref service-ref)
   (rdref record-ref)
   (flags flags-t)
   (rdlen :uint16)
   (data (:const (:pointer :void)))
   (ttl :uint32)))

(define-zeroconf-function (dns-service-remove-record
                           "DNSServiceRemoveRecord")
  ((sdref service-ref)
   (rdref record-ref)
   (flags flags-t)))

(define-zeroconf-function (dns-service-register
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

(fli:define-foreign-callable (%dns-service-register-reply
                              :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (error-code error-t)
     (name (:reference-return dnssd-string :allow-null t))
     (type (:reference-return dnssd-string :allow-null t))
     (domain (:reference-return dnssd-string :allow-null t))
     (context (:pointer :void)))
  (declare (ignore context))
  (with-bound-service-operation operation
    (assert (fli:pointer-eq sdref (operation-handle operation)))
    (let ((registered-service (copy-service
                               (service-operation-service-prototype operation))))
      (setf (service-name registered-service) name
            (service-type registered-service) type
            (service-domain registered-service) domain)
      (service-operation-invoke-callback
       operation
       error-code
       (make-register-result
        :conflict-p (not (flag-test +flag-add+ flags))
        :service registered-service)))))


(define-zeroconf-function (dns-service-enumerate-domains
                           "DNSServiceEnumerateDomains")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (callback (:pointer :function))
   (context (:pointer :void))))


(fli:define-foreign-callable (%dns-service-enumerate-domains-reply
                              :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (interface-index :uint32)
     (error-code error-t)
     (domain-name (:reference-return dnssd-string :allow-null t))
     (context (:pointer :void)))
  (declare (ignore context sdref))
  (with-bound-service-operation operation
    (let ((domain
           (make-domain :interface-index interface-index
                        :name domain-name
                        :defaultp (flag-test +flag-default+ flags))))
      (service-operation-invoke-callback
       operation
       error-code
       (make-enumerate-domains-result
        :more-coming-p (flag-test +flag-more-coming+ flags)
        :presence (flag-test +flag-add+ flags :add :remove)
        :domain domain)))))

(define-zeroconf-function (dns-service-browse
                           "DNSServiceBrowse")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (type (:reference-pass dnssd-string))
   (domain (:reference-pass dnssd-string :allow-null t))
   (callback (:pointer :function))
   (context (:pointer :void))))

(fli:define-foreign-callable (%dns-service-browse-reply
                              :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (interface-index :uint32)
     (error-code error-t)
     (name (:reference-return dnssd-string :allow-null t))
     (type (:reference-return dnssd-string :allow-null t))
     (domain (:reference-return dnssd-string))
     (context (:pointer :void)))
  (declare (ignore context))
  (with-bound-service-operation operation
    (assert (fli:pointer-eq sdref (operation-handle operation)))
    (let ((service
           (make-service :interface-index interface-index
                         :name name
                         :type type
                         :domain domain)))
      (service-operation-invoke-callback
       operation
       error-code
       (make-browse-result
        :more-coming-p (flag-test +flag-more-coming+ flags)
        :presence (flag-test +flag-add+ flags :add :remove)
        :service service)))))

(define-zeroconf-function (dns-service-resolve
                           "DNSServiceResolve")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (name (:reference-pass dnssd-string))
   (type (:reference-pass dnssd-string))
   (domain (:reference-pass dnssd-string :allow-null t))
   (callback (:pointer :function))
   (context (:pointer :void))))

(fli:define-foreign-callable (%dns-service-resolve-reply
                              :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (interface-index :uint32)
     (error-code error-t)
     (full-name (:reference-return dnssd-string :allow-null t))
     (host (:reference-return dnssd-string :allow-null t))
     (port :uint16)
     (txt-record-size :uint16)
     (txt-record-bytes (:pointer (:unsigned :char)))
     (context (:pointer :void)))
  (declare (ignore context))
  (let ((txt-record (fli-make-array-from-bytes txt-record-bytes
                                               txt-record-size)))
    (with-bound-service-operation operation
      (assert (fli:pointer-eq sdref (operation-handle operation)))
      (let* ((service
              (service-operation-service-prototype operation))
             (resolved-service
              (make-service :interface-index interface-index
                            :name (service-name service)
                            :full-name full-name
                            :type (service-type service)
                            :domain (service-domain service)
                            :host host
                            :port (infra:ntohs port)
                            :properties (parse-txt-record txt-record))))
        (service-operation-invoke-callback
         operation
         error-code
         (make-resolve-result
          :more-coming-p (flag-test +flag-more-coming+ flags)
          :service resolved-service))))))

(define-zeroconf-function (dns-service-get-addr-info
                           "DNSServiceGetAddrInfo")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (protocol protocol-t)
   (hostname (:reference-pass dnssd-string))
   (callback (:pointer :function))
   (context (:pointer :void))))

(fli:define-foreign-callable (%dns-service-get-addr-info-reply
                              :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (interface-index :uint32)
     (error-code error-t)
     (hostname (:reference-return dnssd-string :allow-null t))
     (addr (:const (:pointer (:struct sockaddr))))
     (ttl :uint32)
     (context (:pointer :void)))
  (declare (ignore context ))
  (let ((address (when (= error-code +no-err+)
                   (fli-sockaddr-to-string addr))))
    (with-bound-service-operation operation
      (assert (fli:pointer-eq sdref (operation-handle operation)))
      (service-operation-invoke-callback
       operation
       error-code
       (make-get-addr-info-result
        :invalid-p (flag-test +flag-add+ flags :add :invalid)
        :more-coming-p (flag-test +flag-more-coming+ flags)
        :interface-index interface-index
        :hostname hostname
        :address address
        :ttl ttl)))))

(define-zeroconf-function (dns-service-query-record
                           "DNSServiceQueryRecord")
  ((sdref (:pointer service-ref))
   (flags flags-t)
   (interface-index :uint32)
   (full-name (:reference-pass dnssd-string))
   (rrtype :uint16)
   (rrclass :uint16)
   (callback (:pointer :function))
   (context (:pointer :void))))

(fli:define-foreign-callable (%dns-service-query-record-reply
                              :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (interface-index :uint32)
     (error-code error-t)
     (full-name (:reference-return dnssd-string))
     (rrtype :uint16)
     (rrclass :uint16)
     (rdlen :uint16)
     (rdata (:const (:pointer (:unsigned :char))))
     (ttl :uint32)
     (context (:pointer :void)))
  (declare (ignore context sdref))
  (let ((data (fli-make-array-from-bytes rdata
                                         rdlen)))
    (with-bound-service-operation operation
      (service-operation-invoke-callback
       operation
       error-code
       (make-query-record-result
        :presence (flag-test +flag-add+ flags :add :remove)
        :more-coming-p (flag-test +flag-more-coming+ flags)
        :record (make-record :interface-index interface-index
                             :name full-name
                             :type rrtype
                             :class rrclass
                             :data data
                             :ttl ttl))))))
