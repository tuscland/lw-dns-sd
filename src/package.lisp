;;;; package.lisp

(defpackage com.wildora.dnssd
  (:nicknames #:dnssd)
  (:import-from #:com.wildora.dnssd.if-name
   #:if-name-to-index
   #:if-index-to-name
   #:if-name-index)
  (:import-from #:com.wildora.dnssd.conditions
   #:dnssd-error)
  (:import-from #:com.wildora.dnssd.structs
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
   #:domain-defaultp)
  (:import-from #:com.wildora.dnssd.event
   #:event
   #:event-properties
   #:event-property
   #:event-property-error
   #:event-more-coming-p)
  (:import-from #:com.wildora.dnssd.operation
   #:operation-collect-events
   #:operation-wait-event
   #:operation-cancelled-p
   #:*default-event-timeout*
   #:event-timeout-error)
  (:import-from #:com.wildora.dnssd.dispatcher
   #:dispatcher-start
   #:dispatcher-stop
   #:dispatcher-running-p
   #:dispatch
   #:cancel
   #:with-dispatcher)
  (:import-from #:com.wildora.dnssd.api
   #:register
   #:enumerate-domains
   #:browse
   #:resolve
   #:get-addr-info
   #:query-record
   #:nat-port-mapping-create)
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

   #:event
   #:event-properties
   #:event-property
   #:event-property-error
   #:event-more-coming-p

   #:operation
   #:operation-collect-events
   #:operation-wait-event
   #:operation-cancelled-p
   #:*default-event-timeout*
   #:event-timeout-error

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

(defpackage dnssd-user
  (:use #:cl #:dnssd))
