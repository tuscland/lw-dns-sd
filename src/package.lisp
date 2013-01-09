;;;; package.lisp

(defpackage #:com.wildora.dnssd
  (:nicknames #:dnssd)
  (:export
   #:if-name-to-index
   #:if-index-to-name
   #:if-name-index

   #:dnssd-error
   #:result-error
   #:result-error-code

   #:result
   #:result-properties
   #:result-property
   #:result-property-error
   #:result-more-coming-p

   #:operation
   #:operation-canceled-p
   #:operation-next-result
   #:*default-result-timeout*
   #:result-timeout-error

   #:cancel
   #:*default-cancel-timeout*
   #:cancel-timeout-error
   #:create-connection
   #:register
   #:enumerate-domains
   #:browse
   #:resolve
   #:get-addr-info
   #:query-record
   #:enumerate-services-types
   #:nat-port-mapping-create
   #:add-record
   #:update-record
   #:remove-record
   #:register-record
   #:reconfirm-record

   #:dispatcher-start
   #:dispatcher-stop
   #:dispatcher-running-p

   #:build-txt-record
   #:parse-txt-record))

(defpackage #:com.wildora.dnssd-user
  (:nicknames #:dnssd-user)
  (:use #:cl #:dnssd))
