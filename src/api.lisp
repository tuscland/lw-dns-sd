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

;;; Main public API.


(in-package #:com.wildora.dns-sd)


(defun register (port type
                 &key callback
                      interface-index
                      name
                      domain
                      host
                      txt-record
                      no-auto-rename)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-register pointer no-auto-rename interface-index name type domain host port txt-record)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun enumerate-domains (&key callback
                               interface-index
                               (domains :browse-domains))
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-enumerate-domains pointer interface-index domains)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun browse (type
               &key callback
                    interface-index
                    domain)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-browse pointer interface-index type domain)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun resolve (name type domain
                &key callback
                     interface-index
                     broadcasting)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-resolve pointer broadcasting interface-index name type domain)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun get-addr-info (hostname
                      &key callback
                           interface-index
                           protocols
                           broadcasting)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-get-addr-info pointer hostname interface-index protocols broadcasting)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun query-record (full-name rrtype
                     &key callback
                          interface-index
                          (rrclass :IN)
                          broadcasting)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-query-record pointer full-name rrtype rrclass interface-index broadcasting)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun nat-port-mapping-create (internal-port
                                &key callback
                                     interface-index
                                     (external-port 0)
                                     protocols
                                     (ttl 0))
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-nat-port-mapping-create pointer interface-index protocols internal-port external-port ttl)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun create-connection (&key callback)
  (fli:with-dynamic-foreign-objects ((pointer service-ref))
    (dns-service-create-connection pointer)
    (dispatch :handle (fli:dereference pointer)
              :callback callback)))

(defun register-record (operation
                        full-name rrtype rdata
                        &key interface-index
                             (rrclass :IN)
                             identity
                             (ttl 0))
  (fli:with-dynamic-foreign-objects ((pointer record-ref))
    (dns-service-register-record (operation-handle operation)
                                 pointer identity interface-index full-name rrtype rrclass rdata ttl)
    (fli:dereference pointer)))

(defun add-record (operation rrtype rdata
                   &key (ttl 0))
  (fli:with-dynamic-foreign-objects ((pointer record-ref))
    (dns-service-add-record (operation-handle operation)
                            pointer rrtype rdata ttl)
    (fli:dereference pointer)))

(defun update-record (operation record-ref rdata
                      &key (ttl 0))
  (dns-service-update-record (operation-handle operation)
                             record-ref rdata ttl))

(defun remove-record (operation record-ref)
  (dns-service-remove-record (operation-handle operation)
                             record-ref))

(defun reconfirm-record (full-name rrtype rdata interface-index
                         &key (rrclass :IN)
                              force)
  (dns-service-reconfirm-record force interface-index full-name rrtype rrclass rdata))
