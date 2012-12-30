(defpackage com.wildora.dnssd.foreign-high
  (:import-from #:com.wildora.dnssd.operation
   #:current-operation
   #:operation-reply
   #:operation-service-prototype)
  (:import-from #:com.wildora.dnssd.structs
   #:+interface-index-any+
   #:service
   #:make-service
   #:merge-service
   #:service-interface-index
   #:service-name
   #:service-type
   #:service-domain-name
   #:service-host
   #:service-port
   #:service-properties
   #:record
   #:make-record
   #:domain
   #:make-domain
   #:domain-interface-index
   #:domain-name)
  (:import-from #:com.wildora.dnssd.foreign
   #:ip-address-from-sockaddr
   #:sockaddr
   #:service-ref
   #:flags-t
   #:error-t
   #:dnssd-string
   #:%dns-service-register
   #:%dns-service-enumerate-domains
   #:%dns-service-browse
   #:%dns-service-resolve
   #:%dns-service-get-addr-info
   #:%dns-service-query-record)
  (:import-from #:com.wildora.dnssd.txt-record
   #:parse-txt-record
   #:build-txt-record))

(in-package #:com.wildora.dnssd.foreign-high)

(defconstant +flag-more-coming+          #x001)
(defconstant +flag-add+                  #x002)
(defconstant +flag-default+              #x004)

(defun flag-test (flag flags
                  &optional (included-symbol t) (excluded-symbol nil))
  (if (zerop (logand flag flags))
      excluded-symbol
    included-symbol))

(defun flags-get-presence (flags)
  (flag-test +flag-add+ flags :add :remove))

(defun flags-default-p (flags)
  (flag-test +flag-default+ flags))

(defun flags-more-coming-p (flags)
  (flag-test +flag-more-coming+ flags))

(defun make-array-from-foreign-bytes (pointer length)
  (let ((array (make-array length
                           :element-type '(unsigned-byte 8))))
    (fli:replace-foreign-array array
                               pointer
                               :end2 length)))

(fli:define-foreign-callable (dns-service-register-reply
                              :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (error-code error-t)
     (name (:reference-return dnssd-string :allow-null t))
     (type (:reference-return dnssd-string :allow-null t))
     (domain (:reference-return dnssd-string :allow-null t))
     (context (:pointer :void)))
  (declare (ignore context sdref))
  (let* ((operation
          (current-operation))
         (registered-service
          (merge-service (operation-service-prototype operation)
                         :name name
                         :type type
                         :domain-name domain)))
    (operation-reply operation
                     error-code
                     nil
                     :success-p (eq (flags-get-presence flags) :add)
                     :service registered-service)))

(fli:define-foreign-callable (dns-service-enumerate-domains-reply
                              :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (interface-index :uint32)
     (error-code error-t)
     (domain-name (:reference-return dnssd-string :allow-null t))
     (context (:pointer :void)))
  (declare (ignore context sdref))
  (let ((operation
         (current-operation))
        (domain
         (make-domain :interface-index interface-index
                      :name domain-name
                      :defaultp (flags-default-p flags))))
    (operation-reply operation
                     error-code 
                     (flags-more-coming-p flags)
                     :presence (flags-get-presence flags)
                     :domain domain)))

(fli:define-foreign-callable (dns-service-browse-reply
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
  (let ((operation
         (current-operation))
        (service
         (make-service :interface-index interface-index
                       :name name
                       :type type
                       :domain-name domain)))
    (operation-reply operation
                     error-code
                     (flags-more-coming-p flags)
                     :presence (flags-get-presence flags)
                     :service service)))

(fli:define-foreign-callable (dns-service-resolve-reply
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
  (let* ((operation
          (current-operation))
         (txt-record
          (make-array-from-foreign-bytes txt-record-bytes txt-record-size))
         (resolved-service
          (merge-service (operation-service-prototype operation)
                         :interface-index interface-index
                         :full-name full-name
                         :host host
                         :port (infra:ntohs port)
                         :properties (parse-txt-record txt-record))))
    (operation-reply operation
                     error-code
                     (flags-more-coming-p flags)
                     :service resolved-service)))

(fli:define-foreign-callable (dns-service-get-addr-info-reply
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
  (let ((operation
         (current-operation))
        (address
         (when addr;(= error-code +flag-no-err+)
           (ip-address-from-sockaddr addr))))
    (operation-reply operation
                     error-code
                     (flags-more-coming-p flags)
                     :success-p (eq (flags-get-presence flags) :add)
                     :interface-index interface-index
                     :hostname hostname
                     :address address
                     :ttl ttl)))

(fli:define-foreign-callable (dns-service-query-record-reply
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
  (let* ((operation
          (current-operation))
         (data
          (make-array-from-foreign-bytes rdata rdlen))
         (record
          (make-record :interface-index interface-index
                       :name full-name
                       :type rrtype
                       :class rrclass
                       :data data
                       :ttl ttl)))
    (operation-reply operation
                     error-code
                     (flags-more-coming-p flags)
                     :presence (flags-get-presence flags)
                     :record record)))

;;;;
;;;; High level versions of operations functions
;;;;

;;;;
;;;; Keyword options to flags translation
;;;;
(defconstant +flag-no-flag+              #x000)
(defconstant +flag-no-auto-rename+       #x008)

(defconstant +flag-shared+               #x010)
(defconstant +flag-unique+               #x020)

(defconstant +flag-browse-domains+       #x040)
(defconstant +flag-registration-domains+ #x080)
(defconstant +flag-long-lived-query+     #x100)
(defconstant +flag-force-multicast+      #x400)


(defvar *enumerated-domains-flags*
  `((:registration-domains . ,+flag-registration-domains+)
    (:browse-domains       . ,+flag-browse-domains+)))

(defun broadcasting-option-to-flag (option)
  (or (when (eq option :force-multicast)
        +flag-force-multicast+)
      (when (eq option :long-lived-query)
        +flag-long-lived-query+)
      +flag-no-flag+))


(defun dns-service-register (handle-ptr no-auto-rename service)
  (let ((txt-record (build-txt-record
                     (service-properties service)))
        (flags (if no-auto-rename
                   +flag-no-auto-rename+
                 +flag-no-flag+)))
    (check-type service service)
    (fli:with-dynamic-lisp-array-pointer (txt-ptr txt-record)
      (%dns-service-register handle-ptr
                             flags
                             (service-interface-index service)
                             (service-name service)
                             (service-type service)
                             (service-domain-name service)
                             (service-host service)
                             (infra:htons
                              (service-port service))
                             (length txt-record)
                             txt-ptr
                             (infra:make-callback-pointer 'dns-service-register-reply)
                             nil)))
  (values))

(defun dns-service-enumerate-domains (handle-ptr interface-index domains)
  (check-type interface-index (integer 0))
  (assert (member domains '(:browse-domains :registration-domains)))
  (let ((flags (cdr (assoc domains *enumerated-domains-flags*))))
    (%dns-service-enumerate-domains
     handle-ptr
     flags
     interface-index
     (infra:make-callback-pointer 'dns-service-enumerate-domains-reply)
     nil))
  (values))

(defun dns-service-browse (handle-ptr type domain)
  (check-type type string)
  (when domain
    (check-type domain domain))
  (%dns-service-browse
   handle-ptr
   0
   (if domain
       (domain-interface-index domain)
     +interface-index-any+)
   type
   (when domain
     (domain-name domain))
   (infra:make-callback-pointer 'dns-service-browse-reply)
   nil)
  (values))

(defun dns-service-resolve (handle-ptr service resolve-on-all-interfaces broadcasting)
  (when broadcasting
    (assert (eq broadcasting :force-multicast)))
  (let ((flags (broadcasting-option-to-flag broadcasting))
        (interface-flags (if resolve-on-all-interfaces
                             +interface-index-any+
                           (service-interface-index service))))
    (%dns-service-resolve
     handle-ptr
     flags
     interface-flags
     (service-name service)
     (service-type service)
     (service-domain-name service)
     (infra:make-callback-pointer 'dns-service-resolve-reply)
     nil))
  (values))


(defconstant +protocol-ipv4+             #x001)
(defconstant +protocol-ipv6+             #x002)

(defun dns-service-get-addr-info (handle-ptr hostname interface-index protocol broadcasting)
  (check-type interface-index (integer 0))
  (when broadcasting
    (assert (member broadcasting
                    '(:force-multicast :long-lived-query))))
  (assert (member protocol '(:ipv4 :ipv6)))
  (let ((flags (broadcasting-option-to-flag broadcasting))
        (protocol-flags (or (when (eq protocol :ipv4)
                              +protocol-ipv4+)
                            (when (eq protocol :ipv6)
                              +protocol-ipv6+))))
    (%dns-service-get-addr-info
     handle-ptr
     flags
     interface-index
     protocol-flags
     hostname
     (infra:make-callback-pointer 'dns-service-get-addr-info-reply)
     nil))
  (values))

(defun dns-service-query-record (handle-ptr full-name type class interface-index broadcasting)
  (check-type full-name string)
  (check-type type integer)
  (check-type class integer)
  (check-type interface-index (integer 0))
  (when broadcasting
    (assert (member broadcasting
                    '(:force-multicast :long-lived-query))))
  (let ((flags (broadcasting-option-to-flag broadcasting)))
    (%dns-service-query-record
     handle-ptr
     flags
     interface-index
     full-name
     type
     class
     (infra:make-callback-pointer 'dns-service-query-record-reply)
     nil))
  (values))
