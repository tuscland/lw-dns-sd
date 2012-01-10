;;;; package.lisp

(defpackage #:zeroconf
  (:nicknames #:zc)
  (:use #:cl)
  (:export #:if-name-to-index
           #:if-index-to-name

           #:daemon-version
           #:register
           #:browse
           #:resolve
           #:enumerate-domains
           #:query-record
           #:get-addr-info

           #:responder
           #:responder-callback-function
           #:responder-error-function

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
           #:domain-defaultp

           #:cancel

           #:start
           #:stop

           #:dns-sd-error))

(defpackage :zeroconf-user
  (:use :zeroconf :cl))
