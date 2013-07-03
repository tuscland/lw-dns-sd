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

(defmacro stop-and-clear (place)
  `(when ,place
     (cancel ,place)
     (setf ,place nil)))

(defgeneric in-progress-p (object))
(defgeneric stop (object)
  (:method (object)
   nil))

(defun dns-sd-result-adapter (callback object continuation)
  (lambda (operation result)
    (typecase result
      (error
       (funcall continuation object
                :error result))
      (t
       (funcall callback object
                operation result
                continuation)))))

(defstruct address ()
  protocol ; :ipv4 or :ipv6
  hostname
  ip
  ttl)

(defclass address-lookup ()
  ((interface-index
    :initform (error "INTERFACE-INDEX must be specified.")
    :initarg :interface-index
    :accessor address-lookup-interface-index)
   (hostname
    :initform (error "HOSTNAME must be specified")
    :initarg :hostname
    :accessor address-lookup-hostname)
   (operation
    :initform nil
    :accessor address-lookup-operation)
   (addresses
    :initform nil
    :accessor address-lookup-addresses)))

(defmethod start ((self address-lookup) callback)
  (assert (null (address-lookup-operation self)))
  (setf (address-lookup-addresses self) nil
        (address-lookup-operation self)
        (dns-sd:get-addr-info
         (address-lookup-hostname self)
         :interface-index (address-lookup-interface-index self)
         :callback (dns-sd-result-adapter
                    #'address-lookup-callback self callback))))

(defmethod address-lookup-callback ((self address-lookup) operation result callback)
  (when (eq (dns-sd:result-value result :presence) :add)
    (push (make-address :protocol (dns-sd:result-value result :protocol) 
                        :hostname (dns-sd:result-value result :hostname)
                        :ip (dns-sd:result-value result :address)
                        :ttl (dns-sd:result-value result :ttl))
          (address-lookup-addresses self)))
  (unless (dns-sd:result-more-coming-p result)
    (stop-and-clear
     (address-lookup-operation self))
    (funcall callback :change nil)))

(defmethod address-lookup-find-address ((self address-lookup) protocol)
  (assert (member protocol '(:ipv4 :ipv6)))
  (find protocol
        (address-lookup-addresses self)
        :key #'address-protocol))

(defmethod in-progress-p ((self address-lookup))
  (not (null (address-lookup-operation self))))

(defmethod stop ((self address-lookup))
  (stop-and-clear
   (address-lookup-operation self)))


(defgeneric service-equal (self other)
  (:method (self other)
   nil))

(defclass base-service ()
  ((interface-index
    :initarg :interface-index
    :accessor service-interface-index)
   (name
    :initarg :name
    :accessor service-name)
   (type
    :initarg :type
    :accessor service-type)
   (domain
    :initarg :domain
    :accessor service-domain)))

(defmethod start ((self base-service) callback)
  (values))

(defmethod stop ((self base-service))
  (values))

(defmethod service-equal ((self base-service) (other base-service))
  ;; FIXME: normally we should not use the interface-index, a service
  ;; should be viewed as the same item on any interface-index.
  (and (= (service-interface-index self)
          (service-interface-index other))
       (string= (service-name self)
                (service-name other))
       (string= (service-type self)
                (service-type other))
       (string= (service-domain self)
                (service-domain other))))

(defmethod print-object ((self base-service) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (princ (service-name self) stream)))

(defclass service (base-service)
  ((resolve-operation
    :initform nil
    :accessor service-resolve-operation)
   (full-name
    :initform nil
    :accessor service-full-name)
   (hostname
    :initform nil
    :accessor service-hostname)
   (port
    :initform nil
    :accessor service-port)
   (txt-record
    :initform nil
    :accessor service-txt-record))
  (:extra-initargs
   '(:browse-result)))

(defmethod initialize-instance :after ((self service) &key browse-result)
  (when browse-result
    (assert (eq (dns-sd:result-value browse-result :presence) :add))
    (setf (service-interface-index self) (dns-sd:result-value browse-result :interface-index)
          (service-name self) (dns-sd:result-value browse-result :name)
          (service-type self) (dns-sd:result-value browse-result :type) 
          (service-domain self) (dns-sd:result-value browse-result :domain))))

(defmethod start ((self service) callback)
  (assert (null (service-resolve-operation self)))
  (setf (service-resolve-operation self)
        (dns-sd:resolve
         (service-name self)
         (service-type self)
         (service-domain self)
         :interface-index (service-interface-index self)
         :callback (dns-sd-result-adapter
                    #'service-resolve-callback self callback))))

(defmethod service-resolve-callback ((self service) operation result callback)
  (setf (service-full-name self) (dns-sd:result-value result :full-name)
        (service-hostname self) (dns-sd:result-value result :hostname)
        (service-port self) (dns-sd:result-value result :port)
        (service-txt-record self) (dns-sd:result-value result :txt-record))
  (unless (dns-sd:result-more-coming-p result)
    (stop-and-clear
     (service-resolve-operation self))
    (funcall callback :change nil)))

(defmethod in-progress-p ((self service))
  (not (null (service-resolve-operation self))))

(defmethod stop ((self service))
  (stop-and-clear
   (service-resolve-operation self)))


(defclass browser ()
  ((type
    :initform (error "TYPE must be specified")
    :initarg :type
    :accessor browser-type)
   (operation
    :initform nil
    :accessor browser-browse-operation)
   (services
    :initform nil
    :accessor browser-services)))

(defmethod start ((self browser) callback)
  (assert (null (browser-browse-operation self)))
  (setf (browser-browse-operation self)
        (dns-sd:browse (browser-type self)
                       :callback (dns-sd-result-adapter
                                  #'browser-browse-callback self callback))))

(defmethod browser-browse-callback ((self browser) operation result callback)
  (ecase (dns-sd:result-value result :presence)
    (:add
     (let ((service (make-instance 'service :browse-result result)))
       (push service
             (browser-services self))))
    (:remove
     (let ((service (browser-find-service-from-result self result)))
       (removef (browser-services self)
                service))))
  (unless (dns-sd:result-more-coming-p result)
    (funcall callback :change nil)))

(defmethod browser-find-service-from-result ((self browser) result)
  (find-if (lambda (service)
             ;; FIXME: normally we should not use the interface-index, a service
             ;; should be viewed as the same item on any interface-index.
             ;; FIXME: How to factor this with service-equal?
             (and (= (service-interface-index service) (dns-sd:result-value result :interface-index))
                  (string= (service-name service) (dns-sd:result-value result :name))
                  (string= (service-type service) (dns-sd:result-value result :type))
                  (string= (service-domain service) (dns-sd:result-value result :domain))))
           (browser-services self)))

(defmethod in-progress-p ((self browser))
  (not (null (browser-browse-operation self))))

(defmethod stop ((self browser))
  (stop-and-clear
   (browser-browse-operation self)))
