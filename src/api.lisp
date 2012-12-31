(defpackage com.wildora.dnssd.api 
  (:import-from #:com.wildora.dnssd.dispatcher
   #:dispatch-operation)
  (:import-from #:com.wildora.dnssd.foreign-high
   #:+interface-index-any+
   #:service-ref
   #:dns-service-register
   #:dns-service-enumerate-domains
   #:dns-service-browse
   #:dns-service-resolve
   #:dns-service-get-addr-info
   #:dns-service-query-record
   #:dns-service-nat-port-mapping-create)
  (:import-from #:com.wildora.dnssd.constants
   #:+service-class-IN+
   #:+service-type-PTR+))

(in-package #:com.wildora.dnssd.api)

;;;;
;;;; Main API implementation
;;;;

(defun register (service
                 &key callback
                      no-auto-rename)
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-register handle-ptr
                          no-auto-rename
                          service)
    (dispatch-operation :handle (fli:dereference handle-ptr)
                        :callback callback
                        :service-prototype service)))

(defun enumerate-domains (&key callback
                               (interface-index +interface-index-any+)
                               (domains :browse-domains))
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-enumerate-domains handle-ptr interface-index domains)
    (dispatch-operation :handle (fli:dereference handle-ptr)
                        :callback callback)))

(defun browse (type
               &key callback
                    domain) ; (make-domain :name "local.")
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-browse handle-ptr type domain)
    (dispatch-operation :handle (fli:dereference handle-ptr)
                        :callback callback)))

(defun resolve (service
                &key callback
                     (resolve-on-all-interfaces t)
                     broadcasting)
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-resolve handle-ptr service resolve-on-all-interfaces broadcasting)
    (dispatch-operation :handle (fli:dereference handle-ptr)
                        :callback callback
                        :service-prototype service
                        :cancel-after-reply t)))

(defun get-addr-info (hostname
                      &key callback
                           (interface-index +interface-index-any+)
                           (protocol :ipv4)
                           broadcasting)
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-get-addr-info handle-ptr hostname interface-index protocol broadcasting)
    (dispatch-operation :handle (fli:dereference handle-ptr)
                        :callback callback
                        :cancel-after-reply t)))

(defun query-record (full-name type class
                     &key callback
                          (interface-index +interface-index-any+)
                          broadcasting)
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-query-record handle-ptr full-name type class interface-index broadcasting)
    (dispatch-operation :handle (fli:dereference handle-ptr)
                        :callback callback)))

(defun nat-port-mapping-create (internal-port
                                &key callback
                                     (interface-index +interface-index-any+)
                                     (external-port 0)
                                     protocols
                                     (ttl 0))
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-nat-port-mapping-create handle-ptr interface-index protocols internal-port external-port ttl)
    (dispatch-operation :handle (fli:dereference handle-ptr)
                        :callback callback
                        :cancel-after-reply (and (zerop internal-port)
                                                 (zerop external-port)
                                                 (zerop ttl)
                                                 (null protocols)))))


;;;;
;;;; Handful additions
;;;;

(defvar *meta-query-service-full-name*
  "_services._dns-sd._udp.local.")

(defun query-service-types (&key callback
                                 (interface-index +interface-index-any+))
  (query-record *meta-query-service-full-name*
                +service-type-PTR+
                +service-class-IN+
                :interface-index interface-index
                :callback callback))
