;;;; zeroconf.lisp

(in-package #:zeroconf)


(defconstant +protocol-ipv4+             #x001)
(defconstant +protocol-ipv6+             #x002)

(defconstant +no-err+                    #x000)
(defconstant +flag-more-coming+          #x001)
(defconstant +flag-add+                  #x002)
(defconstant +flag-default+              #x004)
(defconstant +flag-no-auto-rename+       #x008)
(defconstant +flag-shared+               #x010)
(defconstant +flag-unique+               #x020)
(defconstant +flag-browse-domains+       #x040)
(defconstant +flag-registration-domains+ #x080)
(defconstant +flag-force-multicast+      #x400)
(defconstant +flag-no-flag+              #x000)



(defun flags->symbols (definitions flags)
  (remove nil
          (loop for (flag symbol default-symbol) in definitions
                if (zerop (logand (symbol-value flag)
                                  flags))
                collect default-symbol
                else
                collect symbol)))

(defun symbols->flags (definitions symbols)
  (loop with flags = 0
        for (symbol flag) in definitions
        when (member symbol symbols)
        do (setq flags (logior (symbol-value flag)
                               flags))
        finally (return flags)))


;; --------------------------------------------------------------------------------
;; LOW LEVEL
;; --------------------------------------------------------------------------------

(fli:define-opaque-pointer service-ref :void)

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

(fli:define-c-struct (sockaddr
                      (:foreign-name "sockaddr"))
  (sa_len :uint8)
  (sa_family :uint8)
  (sa_data (:pointer :char)))


(fli:define-foreign-function
    (dns-service-deallocate
     "DNSServiceRefDeallocate" :source)
    ((sdref service-ref))
  :result-type :void
  :language :ansi-c)

(fli:define-foreign-function
    (dns-service-sockfd
     "DNSServiceRefSockFD" :source)
    ((sdref service-ref))
  :result-type :int
  :language :ansi-c)


(defmacro def-dnssd-function (name args external-name)
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
	       (dns-sd-error ,result-var)))))))


(def-dnssd-function dns-service-process-result
                    ((sdref service-ref))
                    "DNSServiceProcessResult")

(def-dnssd-function dns-service-register
                    ((sdref (:pointer service-ref))
                     (flags flags-t)
                     (interface-index :uint32)
                     (name (:reference-pass dnssd-string :allow-null t))
                     (type (:reference-pass dnssd-string))
                     (domain (:reference-pass dnssd-string :allow-null t))
                     (host (:reference-pass dnssd-string :allow-null t))
                     (port :uint16)
                     (txtlen :uint16)
                     (txtrecord (:pointer (:const :void)))
                     (callback (:pointer :function))
                     (context (:pointer :void)))
                    "DNSServiceRegister")

(fli:define-foreign-callable
    (%dns-service-register-reply
     :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (error-code error-t)
     (name (:reference-return dnssd-string :allow-null t))
     (type (:reference-return dnssd-string :allow-null t))
     (domain (:reference-return dnssd-string :allow-null t))
     (context (:pointer :void)))
  (declare (ignore context sdref))
  (with-bound-operation operation
    (let ((service (operation-user-info operation)))
      (operation-invoke-callback
       operation
       error-code
       (flags->symbols '((+flag-add+ :add :conflict)) flags)
       (make-service :interface-index (service-interface-index service)
                     :name name
                     :type type
                     :domain domain
                     :host (service-host service)
                     :port (service-port service)
                     :txt-record (service-txt-record service))))))

(defparameter *service-register-reply-pointer*
  (fli:make-pointer :symbol-name '%dns-service-register-reply
                    :functionp t))


(def-dnssd-function dns-service-enumerate-domains
                    ((sdref (:pointer service-ref))
                     (flags flags-t)
                     (interface-index :uint32)
                     (callback (:pointer :function))
                     (context (:pointer :void)))
                    "DNSServiceEnumerateDomains")

(fli:define-foreign-callable
    (%dns-service-enumerate-domains-reply
     :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (interface-index :uint32)
     (error-code error-t)
     (domain (:reference-return dnssd-string :allow-null t))
     (context (:pointer :void)))
  (declare (ignore context sdref))
  (let ((symbols (flags->symbols '((+flag-more-coming+ :more-coming :finished)
                       (+flag-add+         :add         :remove)
                       (+flag-default+     :default     nil))
                     flags)))
    (with-bound-operation operation
      (operation-invoke-callback
       operation
       error-code
       symbols
       (make-domain :interface-index interface-index
                    :name domain
                    :defaultp (member :default symbols))))))

(defparameter *service-enumerate-domains-reply-pointer*
  (fli:make-pointer :symbol-name '%dns-service-enumerate-domains-reply
                    :functionp t))


(def-dnssd-function dns-service-browse
                    ((sdref (:pointer service-ref))
                     (flags flags-t)
                     (interface-index :uint32)
                     (type (:reference-pass dnssd-string))
                     (domain (:reference-pass dnssd-string :allow-null t))
                     (callback (:pointer :function))
                     (context (:pointer :void)))
                    "DNSServiceBrowse")

(fli:define-foreign-callable
    (%dns-service-browse-reply
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
  (with-bound-operation operation
    (operation-invoke-callback
     operation
     error-code
     (flags->symbols '((+flag-more-coming+ :more-coming :finished)
                       (+flag-add+         :add         :remove))
                     flags)
     (make-service :interface-index interface-index
                   :name name
                   :type type
                   :domain domain))))

(defparameter *service-browse-reply-pointer*
  (fli:make-pointer :symbol-name '%dns-service-browse-reply
                    :functionp t))


(def-dnssd-function dns-service-resolve
                    ((sdref (:pointer service-ref))
                     (flags flags-t)
                     (interface-index :uint32)
                     (name (:reference-pass dnssd-string))
                     (type (:reference-pass dnssd-string))
                     (domain (:reference-pass dnssd-string :allow-null t))
                     (callback (:pointer :function))
                     (context (:pointer :void)))
                    "DNSServiceResolve")

(defun fli-make-array-from-bytes (pointer length)
  (let ((array (make-array length
                           :element-type '(unsigned-byte 8)
                           :allocation :static)))
    (fli:replace-foreign-array array
                               pointer
                               :end2 length)))

(fli:define-foreign-callable
    (%dns-service-resolve-reply
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
    (with-bound-operation operation
      (let ((service (operation-user-info operation)))
        (operation-invoke-callback
         operation
         error-code
         (flags->symbols '((+flag-more-coming+ :more-coming :finished))
                         flags)
         (make-service :interface-index interface-index
                       :name (service-name service)
                       :full-name full-name
                       :type (service-type service)
                       :domain (service-domain service)
                       :host host
                       :port (ntohs port)
                       :txt-record (parse-txt-record txt-record)))))))

(defparameter *service-resolve-reply-pointer*
  (fli:make-pointer :symbol-name '%dns-service-resolve-reply
                    :functionp t))

(def-dnssd-function dns-service-get-addr-info
                    ((sdref (:pointer service-ref))
                     (flags flags-t)
                     (interface-index :uint32)
                     (protocol protocol-t)
                     (hostname (:reference-pass dnssd-string))
                     (callback (:pointer :function))
                     (context (:pointer :void)))
                     "DNSServiceGetAddrInfo")

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

(fli:define-foreign-callable
    (%dns-service-get-addr-info-reply
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
    (with-bound-operation operation
      (operation-invoke-callback
       operation
       error-code
       (flags->symbols '((+flag-more-coming+ :more-coming :finished)
                         (+flag-add+         :add         :invalid))
                       flags)
       interface-index
       hostname
       address
       ttl))))

(defparameter *service-get-addr-info-reply-pointer*
  (fli:make-pointer :symbol-name '%dns-service-get-addr-info-reply
                    :functionp t))


;; --------------------------------------------------------------------------------
;; HIGH LEVEL
;; --------------------------------------------------------------------------------

(defmethod register
           ((self responder) service &key (no-auto-rename nil))
  (check-type (service-interface-index service) (integer 0))
  (when (service-name service)
    (check-type (service-name service) string))
  (check-type (service-type service) string)
  (when (service-domain service)
    (check-type (service-domain service) string))
  (when (service-host service)
    (check-type (service-host service) string))
  (check-type (service-port service) (integer 0))
  (check-type (service-txt-record service) list)
  (let ((txt-record (build-txt-record
                     (service-txt-record service))))
    (fli:with-dynamic-foreign-objects ((ptr service-ref))
      (fli:with-dynamic-lisp-array-pointer (txt-ptr txt-record)
        (dns-service-register ptr
                              (if no-auto-rename
                                  +flag-no-auto-rename+
                                +flag-no-flag+)
                              (service-interface-index service)
                              (service-name service)
                              (service-type service)
                              (service-domain service)
                              (service-host service)
                              (htons
                               (service-port service))
                              (length txt-record)
                              txt-ptr
                              *service-register-reply-pointer*
                              nil))
      (dispatch-operation
       (make-instance 'operation
                      :handle (fli:dereference ptr)
                      :responder self
                      :user-info (copy-service service))))))

(defmethod browse
           ((self responder) type
            &key (interface-index 0) (domain nil))
  (check-type type string)
  (check-type interface-index (integer 0))
  (when domain
    (check-type domain string))
  (fli:with-dynamic-foreign-objects ((ptr service-ref))
    (dns-service-browse ptr
                        0
                        interface-index
                        type
                        domain
                        *service-browse-reply-pointer*
                        nil)
    (dispatch-operation
     (make-instance 'operation
                    :handle (fli:dereference ptr)
                    :responder self))))


(defmethod resolve ((self responder) service
                    &key (resolve-on-all-interfaces t) (force-multicast nil))
  (unless resolve-on-all-interfaces
    (check-type (service-interface-index service) (integer 0)))
  (check-type (service-name service) string)
  (check-type (service-type service) string)
  (check-type (service-domain service) string)
  (fli:with-dynamic-foreign-objects ((ptr service-ref))
    (dns-service-resolve ptr
                         (if force-multicast
                             +flag-force-multicast+
                           +flag-no-flag+)
                         (if resolve-on-all-interfaces
                             0
                           (service-interface-index service))
                         (service-name service)
                         (service-type service)
                         (service-domain service)
                         *service-resolve-reply-pointer*
                         nil)
    (dispatch-operation (make-instance 'operation
                                       :handle (fli:dereference ptr)
                                       :responder self
                                       :cancel-after-reply t))))

(defvar *enumerated-domain-flags* `((:registration . ,+flag-registration-domains+)
                                    (:browse       . ,+flag-browse-domains+)))

(defmethod enumerate-domains ((self responder)
                              &key (interface-index 0) (domain :browse))
  (check-type interface-index integer)
  (assert (member domain *enumerated-domain-flags* :key #'car)
      (domain)
    "Keyword argument :DOMAIN ~A is not a member of ~A."
    domain (mapcar #'car *enumerated-domain-flags*))
  (fli:with-dynamic-foreign-objects ((ptr service-ref))
    (dns-service-enumerate-domains ptr
                                   (cdr
                                    (assoc domain *enumerated-domain-flags*))
                                   interface-index
                                   *service-enumerate-domains-reply-pointer*
                                   nil)
    (dispatch-operation
     (make-instance 'operation
                    :handle (fli:dereference ptr)
                    :responder self))))

(defmethod query-record ((self responder) name record-type record-class interface-index)
  )

(defmethod get-addr-info ((self responder) hostname
                          &key (interface-index 0)
                               (protocol +protocol-ipv4+))
  (fli:with-dynamic-foreign-objects ((ptr service-ref))
    (dns-service-get-addr-info ptr
                               0 ; kDNSServiceFlagsForceMulticast
                               interface-index
                               protocol
                               hostname
                               *service-get-addr-info-reply-pointer*
                               nil)
    (let ((operation (make-instance 'operation
                                    :handle (fli:dereference ptr)
                                    :responder self
                                    :cancel-after-reply t)))
      (dispatch-operation operation))))
