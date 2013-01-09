(in-package #:com.wildora.dnssd)

;;;;
;;;; Main API
;;;;

(defun register (port type
                 &key callback
                      interface-index
                      name
                      domain
                      host
                      txt-record
                      no-auto-rename)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-register pointer no-auto-rename interface-index name type domain host port txt-record)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun enumerate-domains (&key callback
                               interface-index
                               (domains :browse-domains))
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-enumerate-domains pointer interface-index domains)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun browse (type
               &key callback
                    interface-index
                    domain)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-browse pointer interface-index type domain)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun resolve (name type domain
                &key callback
                     interface-index
                     (resolve-on-all-interfaces t)
                     broadcasting)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-resolve pointer resolve-on-all-interfaces broadcasting interface-index name type domain)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun get-addr-info (hostname
                      &key callback
                           interface-index
                           (protocol :ipv4)
                           broadcasting)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-get-addr-info pointer hostname interface-index protocol broadcasting)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun query-record (full-name rrtype rrclass
                     &key callback
                          interface-index
                          broadcasting)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-query-record pointer full-name rrtype rrclass interface-index broadcasting)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun nat-port-mapping-create (internal-port
                                &key callback
                                     interface-index
                                     (external-port 0)
                                     protocols
                                     (ttl 0))
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-nat-port-mapping-create pointer interface-index protocols internal-port external-port ttl)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun create-connection (&key callback)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-create-connection pointer)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun register-record (operation
                        full-name rrtype rrclass rdata
                        &key interface-index
                             identity
                             (ttl 0))
  (fli:with-dynamic-foreign-objects ((pointer record-ref))
    (dns-service-register-record (operation-handle operation)
                                 pointer identity interface-index full-name rrtype rrclass rdata ttl)
    (fli:dereference pointer)))

(defun add-record (operation rrtype rdata
                   &key (ttl 0))
  (when (operation-canceled-p operation)
    (error "Cannot add a record with a canceled operation"))
  (fli:with-dynamic-foreign-objects ((pointer record-ref))
    (dns-service-add-record (operation-handle operation)
                            pointer rrtype rdata ttl)
    (fli:dereference pointer)))

(defun update-record (operation record-ref rdata
                      &key (ttl 0))
  (when (operation-canceled-p operation)
    (error "Cannot add a record with a canceled operation"))
  (dns-service-update-record (operation-handle operation)
                             record-ref rdata ttl))

(defun remove-record (operation record-ref)
  (when (operation-canceled-p operation)
    (error "Cannot remove a record with a canceled operation"))
  (dns-service-remove-record (operation-handle operation)
                             record-ref))

(defun reconfirm-record (full-name rrtype rrclass rdata
                         &key interface-index
                              force)
  (dns-service-reconfirm-record force interface-index full-name rrtype rrclass rdata))

;;;;
;;;; Handful additions
;;;;

(defvar *meta-query-services-types*
  "_services._dns-sd._udp.local.")

(defun enumerate-services-types (&key callback
                                      interface-index)
  (query-record *meta-query-services-types*
                +service-type-PTR+
                +service-class-IN+
                :interface-index interface-index
                :callback callback))
