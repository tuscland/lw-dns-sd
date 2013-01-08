(in-package #:com.wildora.dnssd)

;;;;
;;;; Main API
;;;;

(defun register (service
                 &key callback
                      no-auto-rename)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-register pointer
                          no-auto-rename
                          service)
    (dispatch :handle (fli:dereference pointer)
              :callback callback
              :service-prototype service)))

(defun enumerate-domains (&key callback
                               interface-index
                               (domains :browse-domains))
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-enumerate-domains pointer interface-index domains)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun browse (type
               &key callback
                    domain)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-browse pointer type domain)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun resolve (service
                &key callback
                     (resolve-on-all-interfaces t)
                     broadcasting)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-resolve pointer service resolve-on-all-interfaces broadcasting)
    (dispatch :handle (fli:dereference pointer)
              :callback callback
              :service-prototype service
              :cancel-after-reply t)))

(defun get-addr-info (hostname
                      &key callback
                           interface-index
                           (protocol :ipv4)
                           broadcasting)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-get-addr-info pointer hostname interface-index protocol broadcasting)
    (dispatch :handle (fli:dereference pointer)
              :callback callback
              :cancel-after-reply t)))

(defun query-record (full-name type class
                     &key callback
                          interface-index
                          broadcasting)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-query-record pointer full-name type class interface-index broadcasting)
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
              :callback callback
              :cancel-after-reply (and (zerop internal-port)
                                       (zerop external-port)
                                       (zerop ttl)
                                       (null protocols)))))

(defun create-connection (&key callback)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-create-connection pointer)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun register-record (operation
                        full-name type class data
                        &key interface-index
                             identity
                             (ttl 0))
  (fli:with-dynamic-foreign-objects ((pointer record-ref))
    (dns-service-register-record (operation-handle operation)
                                 pointer identity interface-index full-name type class data ttl)
    (fli:dereference pointer)))

(defun add-record (operation type data
                   &key (ttl 0))
  (when (operation-canceled-p operation)
    (error "Cannot add a record on a canceled operation"))
  (fli:with-dynamic-foreign-objects ((pointer record-ref))
    (dns-service-add-record (operation-handle operation)
                            pointer type data ttl)
    (fli:dereference pointer)))

(defun update-record (operation
                      record-ref data
                      &key (ttl 0))
  (when (operation-canceled-p operation)
    (error "Cannot add a record on a canceled operation"))
  (dns-service-update-record (operation-handle operation)
                             record-ref data ttl))

(defun remove-record (operation record-ref)
  (when (operation-canceled-p operation)
    (error "Cannot remove a record on a canceled operation"))
  (dns-service-remove-record (operation-handle operation)
                             record-ref))

(defun reconfirm-record (full-name type class data
                         &key interface-index
                              force)
  (dns-service-reconfirm-record force interface-index full-name type class data))

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
