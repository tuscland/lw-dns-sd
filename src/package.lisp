;;;; package.lisp

(defpackage #:com.wildora.dnssd
  (:nicknames #:dnssd)
  (:export
   #:if-name-to-index
   #:if-index-to-name
   #:if-name-index

   #:dnssd-error

   #:service
   #:service-p
   #:service-equal
   #:copy-service
   #:make-service
   #:merge-service
   #:service-interface-index
   #:service-name
   #:service-full-name
   #:service-type
   #:service-domain-name
   #:service-host
   #:service-port
   #:service-properties

   #:record
   #:record-p
   #:copy-record
   #:make-record
   #:record-interface-index
   #:record-name
   #:record-type
   #:record-class
   #:record-data
   #:record-ttl

   #:domain
   #:domain-p
   #:domain-equal
   #:make-domain
   #:domain-interface-index
   #:domain-name
   #:domain-defaultp

   #:result
   #:result-properties
   #:result-property
   #:result-property-error
   #:result-more-coming-p

   #:operation
   #:operation-collect-results
   #:operation-wait-result
   #:operation-cancelled-p
   #:*default-result-timeout*
   #:result-timeout-error

   #:dispatcher-start
   #:dispatcher-stop
   #:dispatcher-running-p
   #:dispatch
   #:cancel
   #:with-dispatcher
   
   #:register
   #:enumerate-domains
   #:browse
   #:resolve
   #:get-addr-info
   #:query-record
   #:nat-port-mapping-create
   ))

(defpackage #:com.wildora.dnssd-user
  (:nicknames #:dnssd-user)
  (:use #:cl #:dnssd))
