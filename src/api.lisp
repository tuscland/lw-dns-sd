(defpackage #:com.wildora.dnssd.api 
  (:import-from #:com.wildora.dnssd.dispatcher
   #:dispatch)
  (:import-from #:com.wildora.dnssd.core
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
    (dispatch :handle (fli:dereference handle-ptr)
              :callback callback
              :service-prototype service)))

(defun enumerate-domains (&key callback
                               interface-index
                               (domains :browse-domains))
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-enumerate-domains handle-ptr interface-index domains)
    (dispatch :handle (fli:dereference handle-ptr)
              :callback callback)))

(defun browse (type
               &key callback
                    domain)
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-browse handle-ptr type domain)
    (dispatch :handle (fli:dereference handle-ptr)
              :callback callback)))

(defun resolve (service
                &key callback
                     (resolve-on-all-interfaces t)
                     broadcasting)
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-resolve handle-ptr service resolve-on-all-interfaces broadcasting)
    (dispatch :handle (fli:dereference handle-ptr)
              :callback callback
              :service-prototype service
              :cancel-after-reply t)))

(defun get-addr-info (hostname
                      &key callback
                           interface-index
                           (protocol :ipv4)
                           broadcasting)
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-get-addr-info handle-ptr hostname interface-index protocol broadcasting)
    (dispatch :handle (fli:dereference handle-ptr)
              :callback callback
              :cancel-after-reply t)))

(defun query-record (full-name type class
                     &key callback
                          interface-index
                          broadcasting)
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-query-record handle-ptr full-name type class interface-index broadcasting)
    (dispatch :handle (fli:dereference handle-ptr)
              :callback callback)))

(defun nat-port-mapping-create (internal-port
                                &key callback
                                     interface-index
                                     (external-port 0)
                                     protocols
                                     (ttl 0))
  (fli:with-dynamic-foreign-objects ((handle-ptr service-ref))
    (dns-service-nat-port-mapping-create handle-ptr interface-index protocols internal-port external-port ttl)
    (dispatch :handle (fli:dereference handle-ptr)
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

(defun query-services-types (&key callback
                                  interface-index)
  (query-record *meta-query-service-full-name*
                +service-type-PTR+
                +service-class-IN+
                :interface-index interface-index
                :callback callback))
