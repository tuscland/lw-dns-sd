(in-package #:com.wildora.dnssd)

;;;;
;;;; Foreign array support
;;;;
(defmacro with-static-array-pointer ((pointer array) &body body)
  (with-unique-names (static-array)
    `(let ((,static-array (make-array (length ,array)
                                      :element-type '(unsigned-byte 8)
                                      :allocation :static)))
       (fli:replace-foreign-array ,static-array ,array)
       (fli:with-dynamic-lisp-array-pointer (,pointer ,static-array)
         ,@body))))

(defun make-array-from-foreign-bytes (pointer length)
  (let ((array (make-array length :element-type '(unsigned-byte 8))))
    (fli:replace-foreign-array array pointer :end2 length)))


;;;;
;;;; Byte swapping utilities
;;;;
;; From cl-swap
(declaim (inline swap-bytes-16))
(defun swap-bytes-16 (value)
  (declare (type (unsigned-byte 16) value)
           (optimize (speed 3)
                     (safety 0)
                     (hcl:fixnum-safety 0)))
  (logior (ash (logand #xFF value)  8)
          (ash value -8)))

(defun ntohs (value)
  #+:little-endian
  (swap-bytes-16 value)
  #+:big-endian
  value)

(defun htons (value)
  #+:little-endian
  (swap-bytes-16 value)
  #+:big-endian
  value)


;;;;
;;;; Bitfield flag testing
;;;;

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


;;;;
;;;; Foreign callbacks
;;;;

(fli:define-foreign-callable (dns-service-register-reply
                              :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (error-code error-t)
     (name (:reference-return dnssd-string :allow-null t))
     (type (:reference-return dnssd-string :allow-null t))
     (domain (:reference-return dnssd-string :allow-null t))
     (context (:pointer :void)))
  (declare (ignore sdref context))
  (reply error-code
         nil
         :presence (flags-get-presence flags)
         :name name
         :type type
         :domain domain))

(fli:define-foreign-callable (dns-service-enumerate-domains-reply
                              :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (interface-index :uint32)
     (error-code error-t)
     (domain (:reference-return dnssd-string :allow-null t))
     (context (:pointer :void)))
  (declare (ignore sdref context))
  (reply error-code 
         (flags-more-coming-p flags)
         :presence (flags-get-presence flags)
         :defaultp (flags-default-p flags)
         :interface-index interface-index
         :domain domain))

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
  (declare (ignore sdref context))
  (reply error-code
         (flags-more-coming-p flags)
         :presence (flags-get-presence flags)
         :interface-index interface-index
         :name name
         :type type
         :domain domain))

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
  (declare (ignore sdref context))
  (let ((txt-record (parse-txt-record
                     (make-array-from-foreign-bytes txt-record-bytes
                                                    txt-record-size))))
    (reply error-code
           (flags-more-coming-p flags)
           :interface-index interface-index
           :full-name full-name
           :host host
           :port (ntohs port)
           :txt-record txt-record)))

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
  (declare (ignore sdref context))
  (let ((address (when addr
                   (ip-address-from-sockaddr addr))))
    (reply error-code
           (flags-more-coming-p flags)
           :presence (flags-get-presence flags)
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
  (declare (ignore sdref context))
  (let ((data (make-array-from-foreign-bytes rdata rdlen)))
    (reply error-code
           (flags-more-coming-p flags)
           :presence (flags-get-presence flags)
           :interface-index interface-index
           :full-name full-name
           :rrtype rrtype
           :rrclass rrclass
           :rdata data
           :ttl ttl)))

(defun flags-get-protocols (flags)
  (remove-if #'null
             (list (when (flag-test +protocol-tcp+ flags)
                     :tcp)
                   (when (flag-test +protocol-udp+ flags)
                     :udp))))

(defun ip-address-from-int32 (n)
  (flet ((shifted-byte (n c)
           (logand (ash n (- c)) #xFF)))
    (format nil "~A.~A.~A.~A"
            (shifted-byte n 0)
            (shifted-byte n 8)
            (shifted-byte n 16)
            (shifted-byte n 24))))

(fli:define-foreign-callable (dns-service-nat-port-mapping-create-reply
                              :result-type :void)
    ((sdref service-ref)
     (flags flags-t)
     (interface-index :uint32)
     (error-code error-t)
     (external-address :uint32)
     (protocol protocol-t)
     (internal-port :uint16)
     (external-port :uint16)
     (ttl :uint32)
     (context (:pointer :void)))
  (declare (ignore sdref context flags))
  (reply error-code
         nil
         :interface-index interface-index
         :external-address (ip-address-from-int32 external-address)
         :protocols (flags-get-protocols protocol)
         :internal-port internal-port
         :external-port external-port
         :ttl ttl))

(fli:define-foreign-callable (dns-service-register-record-reply
                              :result-type :void)
    ((sdref service-ref)
     (rdref record-ref)
     (flags flags-t)
     (error-code error-t)
     (context (:pointer :void)))
  (declare (ignore sdref context flags))
  (reply error-code
         nil
         :record-ref rdref))

;;;;
;;;; High level versions of operations functions
;;;;

(defun make-callback-pointer (symbol)
  (let ((result (fli:make-pointer :symbol-name symbol
                                  :functionp t)))
    (assert (not (fli:null-pointer-p result)))
    result))

;; FIXME: domain name checks are buggy because the length should
;; account for the *escaped* string, not the input string
(defconstant +max-domain-name-length+ 1008)
(defconstant +max-service-name-length+ 63)

;;;;
;;;; Keyword options to flags translation
;;;;

(defvar *enumerated-domains-flags*
  `((:registration-domains . ,+flag-registration-domains+)
    (:browse-domains       . ,+flag-browse-domains+)))

(defun broadcasting-option-to-flag (option)
  (or (when (eq option :force-multicast)
        +flag-force-multicast+)
      (when (eq option :long-lived-query)
        +flag-long-lived-query+)
      +flag-no-flag+))

(defun dns-service-register (pointer no-auto-rename interface-index name type domain host port txt-record)
  (unless interface-index
    (setf interface-index +interface-index-any+))
  (when name
    (check-type name string)
    (assert (< (length name) +max-service-name-length+)))
  (check-type type string)
  (when domain
    (check-type domain string)
    (assert (< (length domain) +max-domain-name-length+)))
  (when host
    (check-type host string))
  (check-type port (integer 0))
  (when (and no-auto-rename
             (null name))
    (error "NO-AUTO-RENAME cannot be specified in conjunction with the default (nil) service NAME."))
  (assert (< (length txt-record) 65536))
  (unless txt-record
    (setf txt-record (build-txt-record nil)))
  (let ((flags (if no-auto-rename
                   +flag-no-auto-rename+
                 +flag-no-flag+)))
    (with-static-array-pointer (txt-pointer txt-record)
      (%dns-service-register pointer
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
                             nil)))
  (values))

(defun dns-service-enumerate-domains (pointer interface-index domains)
  (unless interface-index
    (setf interface-index +interface-index-any+))
  (check-type interface-index (integer 0))
  (assert (member domains '(:browse-domains :registration-domains)))
  (let ((flags (cdr (assoc domains *enumerated-domains-flags*))))
    (%dns-service-enumerate-domains
     pointer
     flags
     interface-index
     (make-callback-pointer 'dns-service-enumerate-domains-reply)
     nil))
  (values))

(defun dns-service-browse (pointer interface-index type domain)
  (unless interface-index
    (setf interface-index +interface-index-any+)) 
  (check-type type string)
  (when domain
    (check-type domain string))
  (%dns-service-browse
   pointer
   0
   interface-index
   type
   domain
   (make-callback-pointer 'dns-service-browse-reply)
   nil)
  (values))

(defun dns-service-resolve (pointer resolve-on-all-interfaces broadcasting interface-index name type domain)
  (setf interface-index (if resolve-on-all-interfaces
                            +interface-index-any+
                          (or interface-index
                              +interface-index-any+)))
  (when broadcasting
    (assert (eq broadcasting :force-multicast)))
  (check-type name string)
  (check-type type string)
  (check-type domain string)
  (%dns-service-resolve
   pointer
   (broadcasting-option-to-flag broadcasting)
   interface-index
   name
   type
   domain
   (make-callback-pointer 'dns-service-resolve-reply)
   nil)
  (values))

(defun dns-service-get-addr-info (pointer hostname interface-index protocol broadcasting)
  (unless interface-index
    (setf interface-index +interface-index-any+))
  (check-type interface-index (integer 0))
  (check-type hostname string)
  (assert (member protocol '(:ipv4 :ipv6)))
  (when broadcasting
    (assert (member broadcasting
                    '(:force-multicast :long-lived-query))))
  (let ((protocol-flags (or (when (eq protocol :ipv4)
                              +protocol-ipv4+)
                            (when (eq protocol :ipv6)
                              +protocol-ipv6+))))
    (%dns-service-get-addr-info
     pointer
     (broadcasting-option-to-flag broadcasting)
     interface-index
     protocol-flags
     hostname
     (make-callback-pointer 'dns-service-get-addr-info-reply)
     nil))
  (values))

(defun dns-service-query-record (pointer full-name type class interface-index broadcasting)
  (check-type full-name string)
  (check-type type integer)
  (check-type class integer)
  (unless interface-index
    (setf interface-index +interface-index-any+))
  (check-type interface-index (integer 0))
  (when broadcasting
    (assert (member broadcasting
                    '(:force-multicast :long-lived-query))))
  (%dns-service-query-record
   pointer
   (broadcasting-option-to-flag broadcasting)
   interface-index
   full-name
   type
   class
   (make-callback-pointer 'dns-service-query-record-reply)
   nil)
  (values))

(defvar *protocols-flags*
  `((:tcp . ,+protocol-tcp+)
    (:udp . ,+protocol-udp+)))

(defun protocols-to-flags (protocols)
  (loop :with flags := 0
        :for protocol :in protocols
        :do (setf flags (logior flags
                                 (cdr (assoc protocol *protocols-flags*))))
        :finally (return flags)))

(defun dns-service-nat-port-mapping-create (pointer interface-index protocols internal-port external-port ttl)
  (unless interface-index
    (setf interface-index +interface-index-any+))
  (check-type interface-index (integer 0))
  (check-type protocols list)
  (check-type internal-port (integer 0))
  (check-type external-port (integer 0))
  (check-type ttl (integer 0))
  (%dns-service-nat-port-mapping-create
   pointer
   0
   interface-index
   (protocols-to-flags protocols)
   internal-port
   external-port
   ttl
   (make-callback-pointer 'dns-service-nat-port-mapping-create-reply)
   nil)
  (values))

(defun dns-service-register-record (service-ref pointer identity interface-index full-name type class data ttl)
  (assert (not (fli:null-pointer-p service-ref)))
  (assert (member identity '(:shared :unique)))
  (unless interface-index
    (setf interface-index +interface-index-any+))
  (check-type full-name string)
  (check-type type integer)
  (check-type class integer)
  (check-type data (array (unsigned-byte 8)))
  (assert (< (length data) 65536))
  (check-type ttl integer)
  (let ((flags (or (when (eq identity :shared)
                     +flag-shared+)
                   (when (eq identity :unique)
                     +flag-unique+))))
    (with-static-array-pointer (data-pointer data)
      (%dns-service-register-record
       service-ref
       pointer
       flags
       interface-index
       full-name
       type
       class
       (length data)
       data-pointer
       ttl
       (make-callback-pointer 'dns-service-register-record-reply)
       nil)))
  (values))

(defun dns-service-add-record (service-ref record-pointer type data ttl)
  (assert (not (fli:null-pointer-p service-ref)))
  (check-type type integer)
  (check-type data (array (unsigned-byte 8)))
  (assert (< (length data) 65536))
  (check-type ttl integer)
  (with-static-array-pointer (data-pointer data)
    (%dns-service-add-record
     service-ref
     record-pointer
     0
     type
     (length data)
     data-pointer
     ttl))
  (values))

(defun dns-service-update-record (service-ref record-ref data ttl)
  (assert (not (fli:null-pointer-p service-ref)))
  (check-type data (array (unsigned-byte 8)))
  (assert (< (length data) 65536))
  (check-type ttl integer)
  (with-static-array-pointer (data-pointer data)
    (%dns-service-update-record
     service-ref
     record-ref
     0
     (length data)
     data-pointer
     ttl))
  (values))

(defun dns-service-remove-record (service-ref record-ref)
  (assert (not (fli:null-pointer-p service-ref)))
  (%dns-service-remove-record
   service-ref
   record-ref
   0)
  (setf (fli:pointer-address record-ref) 0)
  (values))

(defun dns-service-reconfirm-record (force interface-index full-name type class data)
  (unless interface-index
    (setf interface-index +interface-index-any+))
  (check-type full-name string)
  (check-type type integer)
  (check-type class integer)
  (check-type data (array (unsigned-byte 8)))
  (assert (< (length data) 65536))
  (let ((flags (if force
                   +flag-force+
                 +flag-no-flag+)))
    (with-static-array-pointer (data-pointer data)
      (%dns-service-reconfirm-record
       flags
       interface-index
       full-name
       type
       class
       (length data)
       data-pointer)))
  (values))
