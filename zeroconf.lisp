;;;; zeroconf.lisp
(in-package #:zeroconf)

(defun daemon-version ()
  (fli:with-dynamic-foreign-objects ((result :uint32)
                                     (size :uint32
                                           :initial-element (fli:size-of :uint32)))
    (dns-service-get-property *property-daemon-version* result size)
    (fli:dereference result)))

;;;; Callback: (operation register-result)
(defun register (service
                 &key callback
                      no-auto-rename)                 
  (check-type (service-interface-index service) (integer 0))
  (when (service-name service)
    (check-type (service-name service) string)
    (assert (< (length (service-name service)) +max-service-name-length+)))
  (check-type (service-type service) string)
  (when (service-domain service)
    (check-type (service-domain service) string)
    (assert (< (length (service-domain service)) +max-domain-name-length+)))
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
                              (infra:htons (service-port service))
                              (length txt-record)
                              txt-ptr
                              (infra:make-callback-pointer '%dns-service-register-reply)
                              nil))
      (dispatch
       (make-instance 'service-operation
                      :handle (fli:dereference ptr)
                      :callback callback
                      :service-prototype (copy-service service))))))

(defvar *enumerated-domains-flags* `((:registration-domains . ,+flag-registration-domains+)
                                     (:browse-domains       . ,+flag-browse-domains+)))

;;;; Callback: (operation enumerate-domains-result)
(defun enumerate-domains (&key callback
                               (interface-index +interface-index-any+)
                               (domains :browse-domains))
  (check-type interface-index (integer 0))
  (assert (member domains *enumerated-domains-flags* :key #'car)
      (domains)
    "Keyword argument DOMAINS ~A is not a member of ~A."
    domains (mapcar #'car *enumerated-domains-flags*))
  (fli:with-dynamic-foreign-objects ((ptr service-ref))
    (dns-service-enumerate-domains
     ptr
     (cdr (assoc domains *enumerated-domains-flags*))
     interface-index
     (infra:make-callback-pointer '%dns-service-enumerate-domains-reply)
     nil)
    (dispatch
     (make-instance 'service-operation
                    :handle (fli:dereference ptr)
                    :callback callback))))

;;;; Callback: (operation browse-result)
(defun browse (type
               &key callback
                    (domain (make-domain :name "local.")))
  (check-type type string)
  (when domain
    (check-type domain domain)
    (assert (< (length (domain-name domain)) +max-domain-name-length+)))
  (fli:with-dynamic-foreign-objects ((ptr service-ref))
    (dns-service-browse ptr
                        0
                        (domain-interface-index domain)
                        type
                        (domain-name domain)
                        (infra:make-callback-pointer '%dns-service-browse-reply)
                        nil)
    (dispatch
     (make-instance 'service-operation
                    :handle (fli:dereference ptr)
                    :callback callback))))

;;;; Callback: (operation resolve-result)
(defun resolve (service
                &key callback
                     (resolve-on-all-interfaces t)
                     force-multicast)
  (unless resolve-on-all-interfaces
    (check-type (service-interface-index service) (integer 0)))
  (check-type (service-name service) string)
  (assert (< (length (service-name service)) +max-service-name-length+))
  (check-type (service-type service) string)
  (when (service-domain service)
    (check-type (service-domain service) string)
    (assert (< (length (service-domain service)) +max-domain-name-length+)))
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
                         (infra:make-callback-pointer '%dns-service-resolve-reply)
                         nil)
    (dispatch
     (make-instance 'service-operation
                    :handle (fli:dereference ptr)
                    :callback callback
                    :service-prototype (copy-service service)
                    :cancel-after-reply t))))

;;;; Callback: (operation get-addr-info-result)
(defun get-addr-info (hostname
                      &key callback
                           (interface-index +interface-index-any+)
                           (protocol +protocol-ipv4+)
                           force-multicast
                           long-lived-query)
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
                               (infra:make-callback-pointer '%dns-service-get-addr-info-reply)
                               nil)
    (dispatch
     (make-instance 'service-operation
                    :handle (fli:dereference ptr)
                    :callback callback
                    :cancel-after-reply t))))

;;;; Callback: (operation query-record-result)
(defun query-record (full-name type class
                     &key callback
                          (interface-index +interface-index-any+)
                          force-multicast
                          long-lived-query)
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
                               (infra:make-callback-pointer '%dns-service-query-record-reply)
                               nil)
    (dispatch
     (make-instance 'service-operation
                    :handle (fli:dereference ptr)
                    :callback callback))))

;;;; Callback: see query-record
(defun query-service-types (&key callback
                                 (interface-index +interface-index-any+))
  (query-record callback
                *meta-query-service-full-name*
                +service-type-PTR+
                +service-class-in+
                :interface-index interface-index))
