(defpackage #:com.wildora.dnssd.structs)
(in-package #:com.wildora.dnssd.structs)


(defconstant +interface-index-any+        #x00000000)
(defconstant +interface-index-local-only+ #xFFFFFFFF)
(defconstant +interface-index-unicast+    #xFFFFFFFE)
(defconstant +interface-index-p2p+        #xFFFFFFFD)


(defconstant +max-service-name-length+ 63)
;; FIXME: checks are buggy because the length should account for the
;; *escaped* string, not the input string
(defconstant +max-domain-name-length+ 1008)


(defmacro def-dnssd-struct (name-and-options &rest slots)
  (flet ((read-only-slot (slot)
           (when (atom slot)
             (setf slot (list slot nil)))
           (unless (getf slot :read-only)
             (append slot '(:read-only t)))))
    `(defstruct ,name-and-options
       ,@(mapcar #'read-only-slot slots))))


;; TODO: abstract interface index?
(def-dnssd-struct (service
                      (:constructor %make-service))
  (interface-index +interface-index-any+)
  name
  full-name
  type
  domain-name
  host
  (port 0)
  properties)

(defun validate-service (&key interface-index name full-name type domain-name host port properties)
  (when interface-index
    (check-type interface-index (integer 0)))
  (when name
    (check-type name string)
    (assert (< (length name) +max-service-name-length+)))
  (when full-name
    (check-type full-name string))
  (when type
    (check-type type string))
  (when domain-name
    (check-type domain-name string)
    (assert (< (length domain-name) +max-domain-name-length+)))
  (when host
    (check-type host string))
  (when port
    (check-type port (integer 0)))
  (check-type properties list))

(defun make-service (&rest initargs)
  (apply 'validate-service initargs)
  (apply '%make-service initargs))

(defun merge-service (service
                      &key interface-index
                           name
                           full-name
                           type
                           domain-name
                           host
                           port
                           properties)
  (make-service :interface-index
                (or interface-index
                    (service-interface-index service))
                :name
                (or name
                    (service-name service))
                :full-name
                (or full-name
                    (service-full-name service))
                :type
                (or type
                    (service-type service))
                :domain-name
                (or domain-name
                    (service-domain-name service))
                :host
                (or host
                    (service-host service))
                :port
                (or port
                    (service-port service))
                :properties
                (or properties
                    (service-properties service))))

(defun service-equal (service1 service2)
  "Compares two services by their type, name, domain and interface-index."
  (and (service-p service1)
       (service-p service2)
       (string= (service-type service1)
                (service-type service2))
       (string= (service-name service1)
                (service-name service2))
       (string= (service-domain-name service1)
                (service-domain-name service2))
       (= (service-interface-index service1)
          (service-interface-index service2))))

(def-dnssd-struct record
  (interface-index +interface-index-any+)
  name
  type
  class
  data
  ttl)

(def-dnssd-struct (domain
                   (:constructor %make-domain))
  (interface-index +interface-index-any+)
  name
  defaultp)

(defun validate-domain (&key interface-index
                             name
                             defaultp)
  (when interface-index
    (check-type interface-index (integer 0)))
  (when name
    (assert (< (length name) +max-domain-name-length+)))
  (when defaultp
    (assert (eq defaultp t))))

(defun make-domain (&rest initargs)
  (apply 'validate-domain initargs)
  (apply '%make-domain initargs))

(defun domain-equal (domain1 domain2)
  "Compares two domains by their interface-index and name."
  (and (domain-p domain1)
       (domain-p domain2)
       (string-equal (domain-name domain1)
                     (domain-name domain2))
       (= (domain-interface-index domain1)
          (domain-interface-index domain2))))
