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
(defgeneric in-progress-p (object))
(defgeneric stop (object)
  (:method (object)
   nil))


(defclass object-with-operation ()
  ((operation
    :initform nil
    :accessor operation)
   (callback
    :initform #'do-nothing
    :initarg :callback
    :reader callback)))

(defmethod invoke-callback ((self object-with-operation) reason &optional (object self))
  (funcall (callback self) reason object))

(defmethod report-error ((self object-with-operation) error)
  (invoke-callback self :error error))

(defmethod wrap-operation-callback ((self object-with-operation) callback)
  (lambda (operation result)
    (declare (ignore operation))
    (typecase result
      (error
       (report-error self result))
      (t
       (funcall callback self result)))))

(defmethod start :around ((self object-with-operation))
  (assert (null (operation self)))
  (setf (operation self)
        (call-next-method)))

(defmethod in-progress-p ((self object-with-operation))
  (not (null (operation self))))

(defmethod stop ((self object-with-operation))
  (with-slots (operation) self
    (when operation
      (cancel operation)
      (setf operation nil))))


(defstruct address ()
  protocol ; :ipv4 or :ipv6
  hostname
  ip
  ttl)

(defclass address-lookup (object-with-operation)
  ((interface-index
    :initform (error "INTERFACE-INDEX must be specified.")
    :initarg :interface-index
    :accessor address-lookup-interface-index)
   (hostname
    :initform (error "HOSTNAME must be specified")
    :initarg :hostname
    :accessor address-lookup-hostname)
   (addresses
    :initform nil
    :accessor address-lookup-addresses)))

(defmethod start ((self address-lookup))
  (setf (address-lookup-addresses self) nil)
  (get-addr-info (address-lookup-hostname self)
                 :interface-index (address-lookup-interface-index self)
                 :callback (wrap-operation-callback self #'address-lookup-callback)))

(defmethod address-lookup-callback ((self address-lookup) result)
  (when (eq (result-value result :presence) :add)
    (push (make-address :protocol (result-value result :protocol) 
                        :hostname (result-value result :hostname)
                        :ip (result-value result :address)
                        :ttl (result-value result :ttl))
          (address-lookup-addresses self)))
  (unless (result-more-coming-p result)
    (invoke-callback self :change)))

(defmethod address-lookup-find-address ((self address-lookup) protocol)
  (assert (member protocol '(:ipv4 :ipv6)))
  (find protocol
        (address-lookup-addresses self)
        :key #'address-protocol))


(defgeneric service-equal (self other)
  (:method (self other)
   (and (null self)
        (null other))))

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

(defmethod start ((self base-service))
  nil)

(defmethod in-progress-p ((self base-service))
  nil)

(defmethod service-equal ((self base-service) (other base-service))
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

(defmethod print-object ((self base-service) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (princ (service-name self) stream)))


(defclass service (base-service
                   object-with-operation)
  ((full-name
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
    :accessor service-txt-record)))

(defmethod start ((self service))
  (resolve (service-name self)
           (service-type self)
           (service-domain self)
           :interface-index (service-interface-index self)
           :callback (wrap-operation-callback self #'service-resolve-callback)))

(defmethod service-resolve-callback ((self service) result)
  (setf (service-full-name self) (result-value result :full-name)
        (service-hostname self) (result-value result :hostname)
        (service-port self) (result-value result :port)
        (service-txt-record self) (result-value result :txt-record))
  (unless (result-more-coming-p result)
    (invoke-callback self :change)))


(defclass browser (object-with-operation)
  ((type
    :initform (error "TYPE must be specified")
    :initarg :type
    :accessor browser-type)
   (services
    :initform nil
    :accessor browser-services)
   (service-callback
    :initform #'do-nothing
    :initarg :service-callback
    :reader browser-service-callback)))

(defmethod start ((self browser))
  (browse (browser-type self)
          :callback (wrap-operation-callback self #'browser-callback)))

(defun make-service-from-browse-result (browse-result callback)
  (make-instance 'service
                 :interface-index (result-value browse-result :interface-index)
                 :name (result-value browse-result :name)
                 :type (result-value browse-result :type)
                 :domain (result-value browse-result :domain)
                 :callback callback))

(defmethod browser-callback ((self browser) result)
  (ecase (result-value result :presence)
    (:add
     (push (make-service-from-browse-result result
                                            (browser-service-callback self))
           (browser-services self)))
    (:remove
     (let ((service (find-service-from-result result
                                              (browser-services self))))
       (removef (browser-services self)
                service)
       (stop service))))
  (unless (result-more-coming-p result)
    (invoke-callback self :change)))

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


(defclass registration (object-with-operation)
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

(defmethod start ((self registration))
  (with-slots (port type name domain) self
    (register port type
              :name name
              :domain domain
              :callback (wrap-operation-callback self #'registration-callback))))

(defmethod registration-callback ((self registration) result)
  (with-slots (name domain published-p) self
    (setf name (result-value result :name)
          domain (result-value result :domain)
          published-p (eq (result-value result :presence) :add))
    (invoke-callback self :change)))
