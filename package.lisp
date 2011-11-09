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
           #:service-name
           #:record
           #:domain

           #:cancel
           #:start
           #:stop

           #:dns-sd-error))
