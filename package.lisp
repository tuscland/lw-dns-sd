;;;; package.lisp

(defpackage #:zeroconf
  (:nicknames #:zc)
  (:use #:cl)
  (:export #:if-name-to-index
           #:if-index-to-name

           #:zeroconf-error

           #:start
           #:stop

           #:daemon-version

           #:cancel
           #:register
           #:enumerate-domains
           #:browse
           #:resolve
           #:get-addr-info
           #:query-record

           #:service
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
           #:make-record
           #:record-interface-index
           #:record-name
           #:record-type
           #:record-class
           #:record-data
           #:record-ttl

           #:domain
           #:make-domain
           #:domain-interface-index
           #:domain-name
           #:domain-defaultp))

(defpackage :zeroconf-user
  (:use :zeroconf :cl))
