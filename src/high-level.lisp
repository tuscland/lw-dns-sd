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

;;; High level CLOS API.


(in-package "COM.WILDORA.DNS-SD")

(defgeneric start (object))
(defgeneric stop (object))
(defgeneric running? (object))
(defgeneric handle-result (controller result))
(defgeneric run-operation (controller))

(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))

(defun controller-reply (controller result)
  (handle-result controller result)
  (funcall (controller-callback controller)
           controller))

(defclass controller ()
  ((operation
    :initform nil
    :reader controller-operation)
   (last-error
    :initform nil
    :accessor controller-last-error)
   (reply-mailbox
    :initform (mp:process-mailbox
               (mp:get-current-process))
    :initarg :reply-mailbox
    :accessor controller-reply-mailbox)
   (callback
    :initform nil
    :initarg :callback
    :accessor controller-callback)))

(defmethod start ((self controller))
  (with-slots (operation last-error) self
    (setf last-error nil
          operation (run-operation self))))

(defmethod stop ((self controller))
  (with-slots (operation) self
    (cancel operation)
    (setf operation nil)))

(defmethod handle-result ((self controller) (result error-result))
  (setf (slot-value self 'last-error)
        (error-result-underlying-error result))
  (stop self))


(defstruct address ()
  protocol ; :ipv4 or :ipv6
  hostname
  ip
  ttl)

(defclass address-lookup (controller)
  ((service
    :initform nil
    :initarg :service
    :accessor address-lookup-service)
   (addresses
    :initform nil
    :accessor address-lookup-addresses)))

(defmethod stop :after ((self address-lookup))
  (setf (address-lookup-addresses self) nil))

(defmethod run-operation ((self address-lookup))
  (let ((service (address-lookup-service self)))
    (assert (resolved? service))
    (get-addr-info (service-hostname self)
                   :interface-index (service-interface-index self)
                   :reply-mailbox (controller-reply-mailbox self)
                   :reply-object (curry #'controller-reply self))))

(defmethod handle-result ((self address-lookup) result)
  (when (eq (result-value result :presence) :add)
    (push (make-address :protocol (result-value result :protocol) 
                        :hostname (result-value result :hostname)
                        :ip (result-value result :address)
                        :ttl (result-value result :ttl))
          (address-lookup-addresses self))))

(defmethod address-lookup-find-address ((self address-lookup) protocol)
  (assert (member protocol '(:ipv4 :ipv6)))
  (find protocol
        (address-lookup-addresses self)
        :key #'address-protocol))


(defgeneric service-equal (self other)
  (:method (self other)
   (and (null self)
        (null other))))

(defclass service ()
  ((interface-index
    :initform nil
    :initarg  :interface-index
    :accessor service-interface-index)
   (name
    :initform nil
    :initarg  :name
    :accessor service-name)
   (type
    :initform nil
    :initarg  :type
    :accessor service-type)
   (domain
    :initform nil
    :initarg  :domain
    :accessor service-domain)
   (full-name
    :initform nil
    :initarg  :full-name
    :accessor service-full-name)
   (hostname
    :initform nil
    :initarg  :hostname
    :accessor service-hostname)
   (port
    :initform nil
    :initarg  :port
    :accessor service-port)
   (txt-record
    :initform nil
    :initarg  :txt-record
    :accessor service-txt-record)))

(defmethod service-equal ((self service) (other service))
  ;; FIXME: normally we should not use the interface-index, a service
  ;; should be viewed as the same item on any interface-index.
  (and #+nil
       (= (service-interface-index self) ;Not totally sure at this
                                         ;moment that the
                                         ;interface-index should be
                                         ;used to compare services.
          (service-interface-index other))
       (string= (service-name self)
                (service-name other))
       (string= (service-type self)
                (service-type other))
       (string= (service-domain self)
                (service-domain other))))

(defmethod resolved? ((self service))
  (not (null (service-hostname self))))

(defmethod print-object ((self service) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (princ (service-name self) stream)))


(defclass resolver (controller)
  ((service
    :initform nil
    :initarg :service
    :reader resolver-service)
   (resolved-service
    :initform nil
    :accessor resolver-resolved-service)))

(defmethod run-operation ((self resolver))
  (let ((service (resolver-service self)))
    (resolve (service-name service)
             (service-type self)
             (service-domain self)
             :interface-index (service-interface-index self)
             :reply-mailbox (controller-reply-mailbox self)
             :reply-object (curry #'controller-reply self))))

(defmethod handle-result ((self resolver) result)
  (let ((service (resolver-service self)))
    (setf (resolver-resolved-service self)
          (make-service :interface-index (result-value result :interface-index)
                        :full-name (result-value result :full-name)
                        :hostname (result-value result :hostname)
                        :port (result-value result :port)
                        :txt-record (result-value result :txt-record)
                        :name (service-name service)
                        :type (service-type service)
                        :domain (service-domain service)))))


(defclass browser (controller)
  ((type
    :initform (error "TYPE must be specified")
    :initarg :type
    :accessor browser-type)
   (services
    :initform nil
    :accessor browser-services)))

(defmethod start :before ((self browser))
  (setf (browser-services self) nil))

(defmethod run-operation ((self browser))
  (browse (browser-type self)
          :reply-mailbox (controller-reply-mailbox self)
          :reply-object (curry #'controller-reply self)))

(defun make-service-from-browse-result (browse-result)
  (make-service :interface-index (result-value browse-result :interface-index)
                :name (result-value browse-result :name)
                :type (result-value browse-result :type)
                :domain (result-value browse-result :domain)))

(defmethod handle-result ((self browser) result)
  (ecase (result-value result :presence)
    (:add
     (push (make-service-from-browse-result result)
           (browser-services self)))
    (:remove
     (let ((service (find-service-from-result result (browser-services self))))
       (removef (browser-services self) service)))))

(defun find-service-from-result (result services)
  (find-if (lambda (service)
             ;; FIXME: normally we should not use the interface-index, a service
             ;; should be viewed as the same item on any interface-index.
             ;; FIXME: How to factor this with service-equal?
             (and (= (service-interface-index service) (result-value result :interface-index))
                  (string= (service-name service) (result-value result :name))
                  (string= (service-type service) (result-value result :type))
                  (string= (service-domain service) (result-value result :domain))))
           services))


(defclass registration (controller)
  ((port
    :initform (error "PORT must be specified.")
    :initarg :port
    :reader registration-port)
   (type
    :initform (error "TYPE must be specified.")
    :initarg :type
    :reader registration-type)
   (name
    :initform nil
    :initarg :name
    :reader registration-name)
   (domain
    :initform nil
    :initarg :domain
    :reader registration-domain)
   (published-p
    :initform nil
    :reader registration-published-p)))

(defmethod run-operation ((self registration))
  (with-slots (port type name domain) self
    (register port type
              :name name
              :domain domain
              :reply-mailbox (controller-reply-mailbox self)
              :reply-object (curry #'controller-reply self))))

(defmethod stop :after ((self registration))
  (setf (slot-value self 'published-p) nil))

(defmethod handle-result ((self registration) result)
  (with-slots (name domain published-p) self
    (setf name (result-value result :name)
          domain (result-value result :domain)
          published-p (eq (result-value result :presence) :add))))
