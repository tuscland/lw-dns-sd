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
  (:import-from #:com.wildora.dnssd.result
   #:result
   #:result-property
   #:result-more-coming-p
   #:reply-result
   #:error-result)
  (:import-from #:com.wildora.dnssd.operation
   #:operation
   #:operation-collect-results
   #:operation-wait-result
   #:operation-cancelled-p
   #:*default-result-timeout*
   #:operation-timeout-error)
  (:import-from #:com.wildora.dnssd.dispatcher
   #:dispatcher
   #:start
   #:stop
   #:running-p
   #:cancel)
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

   #:result
   #:result-property
   #:result-more-coming-p
   #:reply-result
   #:error-result

   #:operation
   #:operation-collect-results
   #:operation-wait-result
   #:operation-cancelled-p
   #:*default-result-timeout*
   #:operation-timeout-error

   #:dispatcher
   #:start
   #:stop
   #:running-p
   #:cancel

   #:register
   #:enumerate-domains
   #:browse
   #:resolve
   #:get-addr-info
   #:query-record
   #:nat-port-mapping-create

   #|           
           #:daemon-version

           #:result
           #:error-result

           #:+interface-index-any+|#
   ))

(defpackage dnssd-user
  (:use #:cl #:dnssd))
