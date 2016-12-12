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

(in-package "CL-USER")

(require "comm")

(defpackage "COM.WILDORA.DNS-SD"
  (:nicknames "DNS-SD")
  (:export
   "*DEFAULT-TIMEOUT*"

   ;; Main API
   "DAEMON-VERSION"
   "REGISTER"
   "ENUMERATE-DOMAINS"
   "BROWSE"
   "RESOLVE"
   "GET-ADDR-INFO"
   "QUERY-RECORD"
   "ENUMERATE-SERVICES-TYPES"
   "NAT-PORT-MAPPING-CREATE"
   "CREATE-CONNECTION"
   "ADD-RECORD"
   "UPDATE-RECORD"
   "REMOVE-RECORD"
   "REGISTER-RECORD"
   "RECONFIRM-RECORD"

   ;; Operations
   "OPERATION-REPLY-MAILBOX"
   "CANCEL"

   ;; Results
   "RESULT"
   "RESULT-OPERATION"
   "RESULT-TIME"
   "RESULT-VALUES"
   "RESULT-VALUE"
   "RESULT-MORE-COMING-P"
   "ERROR-RESULT"
   "ERROR-RESULT-UNDERLYING-ERROR"

   ;; Dispatch
   "START-DISPATCH"
   "STOP-DISPATCH"
   "DISPATCH-RUNNING-P"
   
   ;; NETWORK INTERFACE NAME TO INDEX
   "IF-NAME-TO-INDEX"
   "IF-INDEX-TO-NAME"
   "IF-NAME-INDEX"
   "+INTERFACE-INDEX-ANY+"
   "+INTERFACE-INDEX-LOCAL-ONLY+"
   "+INTERFACE-INDEX-UNICAST+"
   "+INTERFACE-INDEX-P2P+"

   ;; Record data handling
   "STRING-TO-BYTES"
   "BYTES-TO-STRING"
   "BUILD-TXT-RECORD"
   "PARSE-TXT-RECORD"

   ;; Utility
   "CONSTRUCT-FULL-NAME"

   ;; ERRORS
   "DNS-SD-ERROR"
   "LIBRARY-NOT-AVAILABLE-ERROR"
   "TIMEOUT-ERROR"
   "RESULT-ERROR"
   "RESULT-ERROR-CODE"
   "ERROR-CODE-DESCRIPTION"

   ;; High level CLOS API
   "START"
   "STOP"
   "IN-PROGRESS-P"

   "REGISTRATION"
   "REGISTRATION-PORT"
   "REGISTRATION-TYPE"
   "REGISTRATION-NAME"
   "REGISTRATION-DOMAIN"

   "BROWSER"
   "BROWSER-SERVICES"

   "BASE-SERVICE" ;TODO: don't export this
   "SERVICE"
   "SERVICE-EQUAL"
   "SERVICE-INTERFACE-INDEX"
   "SERVICE-NAME"
   "SERVICE-TYPE"
   "SERVICE-DOMAIN"
   "SERVICE-FULL-NAME"
   "SERVICE-HOSTNAME"
   "SERVICE-PORT"
   "SERVICE-TXT-RECORD"

   "ADDRESS-LOOKUP"
   "ADDRESS-LOOKUP-HOSTNAME"
   "ADDRESS-LOOKUP-ADDRESSES"
   "ADDRESS-LOOKUP-FIND-ADDRESS"

   "ADDRESS"
   "ADDRESS-PROTOCOL"
   "ADDRESS-HOSTNAME"
   "ADDRESS-IP"
   "ADDRESS-TTL"))
