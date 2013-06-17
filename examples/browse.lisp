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

;;; Example code for asynchronous service browsing using the CAPI

;; This example demonstrates the use of asynchronous operation and how
;; to integrate DNS-SD with the CAPI.
;;
;; To try it, compile and load this file and execute:
;;
;;      (CL-USER::BROWSER-EXAMPLE)

(load (current-pathname "../defsystem"))
(compile-system "DNS-SD" :load t)

(defun callback-with-interface (interface function)
  (lambda (operation result)
    (capi:execute-with-interface-if-alive
     interface
     function interface operation result)))

(defun enumerate-service-types (&key callback
                                      interface-index)
  (dns-sd:query-record "_services._dns-sd._udp.local." :PTR
                       :callback callback
                       :interface-index interface-index))

(defun display-message (message interface)
  (setf (capi:titled-object-message interface)
        message))

(defun first-elt (sequence)
  (when (plusp (length sequence))
    (elt sequence 0)))

(capi:define-interface browser ()
  ((query-operation
    :initform nil
    :accessor browser-query-operation)
   (service-types
    :initform nil
    :accessor browser-service-types)
   (browse-operation
    :initform nil
    :accessor browser-browse-operation)
   (services
    :initform nil
    :accessor browser-services))
  (:panes
   (types-menu
    capi:option-pane
    :selection-callback 'browser-select-type
    :reader browser-types-pane)
   (services-list
    capi:list-panel
    :reader browser-services-list))
  (:layouts
   (main
    capi:column-layout
    '(types-menu services-list)
    :default t))
  (:default-initargs
   :create-callback 'browser-create
   :destroy-callback 'browser-destroy
   :best-height 200
   :best-width 300
   :message-area t))

(defun browser-select-type (type interface)
  (display-message (format nil "Browsing ~A services ..." type)
                   interface)
  (with-accessors ((operation browser-browse-operation)
                   (services browser-services)
                   (services-list browser-services-list)) interface
    (when operation
      (dns-sd:cancel operation)
      (setf (capi:collection-items services-list) nil
            services nil))
    (when type
      (setf operation
            (dns-sd:browse type
                           :callback (callback-with-interface interface
                                                              #'browse-callback))))))

(defun update-service-types (interface)
  (with-accessors ((types-pane browser-types-pane)
                   (types browser-service-types)) interface
    (setf (capi:collection-items types-pane)
          (sort types #'string-lessp))
    (unless (browser-browse-operation interface)
      (when-let (type (first-elt
                       (capi:collection-items types-pane)))
        (setf (capi:choice-selected-item types-pane) type)
        (browser-select-type type interface)))))

(defun update-services (interface)
  (with-accessors ((services-list browser-services-list)) interface
    (setf (capi:collection-items services-list)
          (sort (browser-services interface)
                #'string-lessp))))

(defun handle-error (interface operation error)
  (dns-sd:cancel operation)
  (display-message (format nil "Error occurred: ~A" error)
                   interface))

(defmethod browse-callback (interface operation (error error))
  (handle-error interface operation error))

(defmethod browse-callback (interface operation result)
  (let ((name (dns-sd:result-value result :name)))
    (with-accessors ((services browser-services)) interface
      (case (dns-sd:result-value result :presence)
        (:add
         (pushnew name services :test 'string=))
        (:remove
         (setf services
               (remove name services :test 'string=))))))
  (unless (dns-sd:result-more-coming-p result)
    (update-services interface)))

(defmethod enumerate-service-types-callback (interface operation (error error))
  (handle-error interface operation error))

(defmethod enumerate-service-types-callback (interface operation result)
  (let* ((record (mapcar #'car
                         (dns-sd:parse-txt-record
                          (dns-sd:result-value result :rdata))))
         (type (format nil "~A.~A"
                       (first record)
                       (second record))))
    (with-accessors ((types browser-service-types)) interface
      (case (dns-sd:result-value result :presence)
        (:add
         (pushnew type types :test 'string=))
        (:remove
         (setf types 
               (remove type types :test 'string=))))))
  (unless (dns-sd:result-more-coming-p result)
    (update-service-types interface)))

(defun browser-create (interface)
  (setf (browser-query-operation interface)
        (enumerate-service-types
         :callback (callback-with-interface interface
                                            #'enumerate-service-types-callback))))

(defun browser-destroy (interface)
  (browser-select-type nil interface)
  (dns-sd:cancel
   (browser-query-operation interface)))


(defun browser-example ()
  (capi:contain 'browser))
