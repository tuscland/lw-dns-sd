;;;; package.lisp

(defpackage #:zeroconf
  (:nicknames #:zc)
  (:use #:cl)
  (:export #:if-name-to-index
           #:if-index-to-name
           #:if-name-index

           #:zeroconf-error

           #:start
           #:stop
           #:running-p

           #:daemon-version

           #:cancel
           #:register
           #:enumerate-domains
           #:browse
           #:resolve
           #:get-addr-info
           #:query-record

           #:service
           #:service-p
           #:service-equal
           #:copy-service
           #:make-service
           #:service-interface-index
           #:service-name
           #:service-full-name
           #:service-type
           #:service-domain
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

           #:operation
           #:operation-next-result
           #:operation-collect-results
           #:operation-cancelled-p
           #:with-operation
           #:*default-operation--timeout*

           #:error-result
           #:error-result-p
           #:error-result-condition
           #:resolve-result
           #:resolve-result-p
           #:resolve-result-more-coming-p
           #:resolve-result-service
           #:register-result
           #:register-result-p
           #:register-result-conflict-p
           #:register-result-service
           #:enumerate-domains-result
           #:enumerate-domains-result-p
           #:enumerate-domains-result-more-coming-p
           #:enumerate-domains-result-presence
           #:enumerate-domains-result-domain
           #:browse-result
           #:browse-result-p
           #:browse-result-more-coming-p
           #:browse-result-presence
           #:browse-result-service
           #:get-addr-info-result
           #:get-addr-info-result-p
           #:get-addr-info-result-more-coming-p
           #:get-addr-info-result-invalid-p
           #:get-addr-info-result-interface-index
           #:get-addr-info-result-hostname
           #:get-addr-info-result-address
           #:get-addr-info-result-ttl
           #:query-record-result
           #:query-record-result-p
           #:query-record-result-more-coming-p
           #:query-record-result-presence
           #:query-record-result-record

           #:+interface-index-any+

           #:debugging-callback))

(defpackage :zeroconf-user
  (:use :zeroconf :cl))
