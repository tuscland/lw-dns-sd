;;;; zeroconf.lisp

(in-package #:zeroconf)


(defun daemon-version ()
  (fli:with-dynamic-foreign-objects ((result :uint32)
                                     (size :uint32))
    (dns-service-get-property *property-daemon-version*
                              result
                              size)
    (fli:dereference result)))

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
  (check-type (service-properties service) list)
  (let ((txt-record (build-txt-record
                     (service-properties service))))
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
      (dispatch
       (make-instance 'service-handle
                      :ref (fli:dereference ptr)
                      :responder self
                      :user-info (copy-service service))))))

(defmethod browse
           ((self responder)
            type
            &key (interface-index 0)
                 (domain nil))
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
    (dispatch
     (make-instance 'service-handle
                    :ref (fli:dereference ptr)
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
    (dispatch
     (make-instance 'service-handle
                    :ref (fli:dereference ptr)
                    :responder self
                    :cancel-after-reply t))))

(defvar *enumerated-domain-flags* `((:registration . ,+flag-registration-domains+)
                                    (:browse       . ,+flag-browse-domains+)))

(defmethod enumerate-domains ((self responder)
                              &key (interface-index 0) (domain :browse))
  (check-type interface-index (integer 0))
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
    (dispatch
     (make-instance 'service-handle
                    :ref (fli:dereference ptr)
                    :responder self))))

(defmethod query-record ((self responder) full-name type class
                         &key (interface-index 0)
                              (force-multicast nil)
                              (long-lived-query nil))
  (check-type full-name string)
  (check-type type integer)
  (check-type class integer)
  (check-type interface-index (integer 0))
  (fli:with-dynamic-foreign-objects ((ptr service-ref))
    (dns-service-query-record ptr
                              (or (when force-multicast
                                    +flag-force-multicast+)
                                  (when long-lived-query
                                    +flag-long-lived-query+)
                                  0)
                               interface-index
                               full-name
                               type
                               class
                               *service-query-record-reply-pointer*
                               nil)
    (dispatch
     (make-instance 'service-handle
                    :ref (fli:dereference ptr)
                    :responder self))))

(defmethod query-service-types ((self responder) &key (interface-index 0))
  (query-record self
                *meta-query-service-full-name*
                +service-type-PTR+
                +service-class-in+
                :interface-index interface-index))

(defmethod get-addr-info ((self responder) hostname
                          &key (interface-index 0)
                               (protocol +protocol-ipv4+)
                               (force-multicast nil)
                               (long-lived-query nil))
  (fli:with-dynamic-foreign-objects ((ptr service-ref))
    (dns-service-get-addr-info ptr
                               (or (when force-multicast
                                    +flag-force-multicast+)
                                  (when long-lived-query
                                    +flag-long-lived-query+)
                                  0)
                               interface-index
                               protocol
                               hostname
                               *service-get-addr-info-reply-pointer*
                               nil)
    (dispatch
     (make-instance 'service-handle
                    :ref (fli:dereference ptr)
                    :responder self
                    :cancel-after-reply t))))
