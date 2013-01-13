;;;; -*- encoding: utf-8; mode: LISP; syntax: COMMON-LISP; indent-tabs-mode: nil -*-

;;; DNS Service Discovery for LispWorks.
;;; Copyright (c) 2013, Camille Troillard. All rights reserved.

;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing,
;;; software distributed under the License is distributed on an "AS
;;; IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
;;; express or implied.  See the License for the specific language
;;; governing permissions and limitations under the License.


(require "comm")

(defpackage #:com.wildora.dns-sd
  (:nicknames #:dns-sd)
  (:export
   *library-version*

   ;; Network interface name to index
   #:if-name-to-index
   #:if-index-to-name
   #:if-name-index
   #:+interface-index-any+
   #:+interface-index-local-only+
   #:+interface-index-unicast+
   #:+interface-index-p2p+

   ;; Record data handling
   #:string-to-bytes
   #:bytes-to-string
   #:build-txt-record
   #:parse-txt-record

   ;; Utility
   #:construct-full-name

   ;; Errors
   #:dns-sd-error
   #:result-error
   #:result-error-code

   ;; Results
   #:result
   #:result-values
   #:result-value
   #:result-more-coming-p
   #:error-result
   #:error-result-error

   ;; Operations
   #:operation-next-result
   #:*default-result-timeout*
   #:result-timeout-error
   #:cancel
   #:*default-cancel-timeout*
   #:cancel-timeout-error

   ;; Dispatch
   #:start-dispatch
   #:stop-dispatch
   #:dispatch-running-p

   ;; Main API
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
   #:create-connection))

(defpackage #:com.wildora.dns-sd-user
  (:nicknames #:dns-sd-user)
  (:use #:cl #:dns-sd))
