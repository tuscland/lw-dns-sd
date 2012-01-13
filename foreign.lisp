(in-package #:zeroconf)

(defun make-callable-pointer (symbol)
  (fli:make-pointer :symbol-name symbol
                    :functionp t))

(defun fli-make-array-from-bytes (pointer length)
  (let ((array (make-array length
                           :element-type '(unsigned-byte 8)
                           :allocation :static)))
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


(defmacro def-dnssd-function ((name external-name) args)
  "Declares a foreign function that returns a value of
  type DNSServiceErrorType and defines a wrapper around the
  foreign function that raises an error of type
  DNS-SD-RESULT-ERROR if the foreign function returns a value
  indicating that an error occurred."
  (let ((unwrapped-name (intern (format nil "%~A" name)))
	(result-var (gensym "RESULT"))
        (arg-names (mapcar #'car args)))
    `(dspec:def (def-dnssd-function ,name)
       (fli:define-foreign-function (,unwrapped-name ,external-name :source)
	   ,args
	 :result-type error-t
         :language :ansi-c)
       (defun ,name ,arg-names
	 (let ((,result-var (,unwrapped-name ,@arg-names)))
	   (if (= ,result-var +no-err+)
	       ,result-var
             (raise-dns-sd-error ,result-var)))))))

(editor:setup-indent "def-dnssd-function" 1)

(def-dnssd-function (dns-service-get-property
                     "DNSServiceGetProperty")
  ((property (:reference-pass dnssd-string))
   (result (:pointer :void))
   (size (:pointer :uint32))))

(def-dnssd-function (dns-service-process-result
                     "DNSServiceProcessResult")
  ((sdref service-ref)))

(def-dnssd-function (dns-service-add-record
                     "DNSServiceAddRecord")
  ((sdref service-ref)
   (rdref (:pointer record-ref))
   (flags flags-t)
   (rrtype :uint16)
   (rdlen :uint16)
   (data (:const (:pointer :void)))
   (ttl :uint32)))

(def-dnssd-function (dns-service-update-record
                     "DNSServiceUpdateRecord")
  ((sdref service-ref)
   (rdref record-ref)
   (flags flags-t)
   (rdlen :uint16)
   (data (:const (:pointer :void)))
   (ttl :uint32)))

(def-dnssd-function (dns-service-remove-record
                     "DNSServiceRemoveRecord")
  ((sdref service-ref)
   (rdref record-ref)
   (flags flags-t)))

(def-dnssd-function (dns-service-register
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
  (declare (ignore context sdref))
  (with-bound-service-handle handle
    (let ((service (service-handle-user-info handle)))
      (service-handle-invoke-callback
       handle
       error-code
       (test-flag +flag-add+ :add :conflict flags)
       (make-service :interface-index (service-interface-index service)
                     :name name
                     :type type
                     :domain domain
                     :host (service-host service)
                     :port (service-port service)
                     :properties (service-properties service))))))

(def-dnssd-function (dns-service-enumerate-domains
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
     (domain (:reference-return dnssd-string :allow-null t))
     (context (:pointer :void)))
  (declare (ignore context sdref))
  (let ((defaultp (test-flag +flag-default+ t nil flags)))
    (with-bound-service-handle handle
      (service-handle-invoke-callback
       handle
       error-code
       (test-flag +flag-add+ :add :remove flags)
       defaultp
       (test-flag +flag-more-coming+ t nil flags)
       (make-domain :interface-index interface-index
                    :name domain
                    :defaultp defaultp)))))

(def-dnssd-function (dns-service-browse
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
  (declare (ignore context sdref))
  (with-bound-service-handle handle
    (service-handle-invoke-callback
     handle
     error-code
     (test-flag +flag-add+ :add :remove flags)
     (test-flag +flag-more-coming+ t nil flags)
     (make-service :interface-index interface-index
                   :name name
                   :type type
                   :domain domain))))

(def-dnssd-function (dns-service-resolve
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
  (declare (ignore context sdref))
  (let ((txt-record (fli-make-array-from-bytes txt-record-bytes
                                               txt-record-size)))
    (with-bound-service-handle handle
      (let ((service (service-handle-user-info handle)))
        (service-handle-invoke-callback
         handle
         error-code
         (test-flag +flag-more-coming+ t nil flags)
         (make-service :interface-index interface-index
                       :name (service-name service)
                       :full-name full-name
                       :type (service-type service)
                       :domain (service-domain service)
                       :host host
                       :port (ntohs port)
                       :properties (parse-txt-record txt-record)))))))

(def-dnssd-function (dns-service-get-addr-info
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
  (declare (ignore context sdref))
  (let ((address (when (= error-code +no-err+)
                   (fli-sockaddr-to-string addr))))
    (with-bound-service-handle handle
      (service-handle-invoke-callback
       handle
       error-code
       (test-flag +flag-add+ :add :invalid flags)
       (test-flag +flag-more-coming+ t nil flags)
       interface-index
       hostname
       address
       ttl))))

(def-dnssd-function (dns-service-query-record
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
    (with-bound-service-handle handle
      (service-handle-invoke-callback
       handle
       error-code
       (test-flag +flag-add+ :add :remove flags)
       (test-flag +flag-more-coming+ t nil flags)
       (make-record :interface-index interface-index
                    :name full-name
                    :type rrtype
                    :class rrclass
                    :data data
                    :ttl ttl)))))
