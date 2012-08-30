(in-package #:zeroconf)

(defstruct service
  (interface-index +interface-index-any+)
  name
  full-name
  type
  domain
  host
  (port 0)
  properties)

(defun service-equal (service1 service2)
  "Compares two services by their type, name, domain and interface-index."
  (and (service-p service1)
       (service-p service2)
       (string-equal (service-type service1)
                     (service-type service2))
       (string-equal (service-name service1)
                     (service-name service2))
       (string-equal (service-domain service1)
                     (service-domain service2))
       (= (service-interface-index service1)
          (service-interface-index service2))))

(defstruct record
  (interface-index +interface-index-any+)
  name
  type
  class
  data
  ttl)

(defstruct domain
  (interface-index +interface-index-any+)
  name
  defaultp)

(defun domain-equal (domain1 domain2)
  "Compares two domains by their interface-index and name."
  (and (domain-p domain1)
       (domain-p domain2)
       (string-equal (domain-name domain1)
                     (domain-name domain2))
       (= (domain-interface-index domain1)
          (domain-interface-index domain2))))
