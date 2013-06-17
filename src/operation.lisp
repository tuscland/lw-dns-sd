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

;;; DNS-SD operations.
;;; A call to a function of the main API (defined in api.lisp) results
;;; in an operation waiting in the dispatch queue.


(in-package "COM.WILDORA.DNS-SD")

(defun daemon-version ()
  (fli:with-dynamic-foreign-objects ((result :uint32)
                                     (size :uint32
                                           :initial-element (fli:size-of :uint32)))
    (dns-service-get-property "DaemonVersion" result size)
    (fli:dereference result)))

(defclass operation (comm:socket-stream)
  ((handle
    :accessor operation-handle
    :initform (error "HANDLE must be specified")
    :initarg :handle)
   (callback
    :reader operation-callback
    :initform nil
    :initarg :callback)
   (results-queue
    :reader operation-results-queue
    :initform (mp:make-mailbox)))
  (:default-initargs
   :direction :input))

(defmethod initialize-instance :after ((self operation) &key handle)
  (setf (comm:socket-stream-socket self)
        (dns-service-sockfd handle)))

(defmethod cancel-operation ((self operation))
  (if (operation-canceled-p self)
      (warn "Operation ~A has already been canceled" self)
    (progn
      (dns-service-deallocate (operation-handle self))
      (setf (fli:pointer-address
             (operation-handle self)) 0))))

(defmethod operation-canceled-p ((self operation))
  (fli:null-pointer-p
   (operation-handle self)))

(defmethod operation-enqueue-result ((self operation) result)
  (mp:mailbox-send
   (operation-results-queue self)
   result))

(defmethod operation-invoke-callback ((self operation) result)
  (funcall (or (operation-callback self)
               #'operation-enqueue-result)
           self result))


(declaim (special-dynamic *current-operation*))

(defmethod reply (error-code more-coming-p &rest result-values)
  (maybe-signal-result-error error-code)
  (operation-invoke-callback *current-operation*
                             (make-result more-coming-p
                                          result-values)))

(defmethod process-result ((self operation))
  "Called from the dispatched to process pending results."
  (handler-bind ((result-error
                  #'(lambda (condition)
                      (operation-invoke-callback self condition))))
    (let ((*current-operation* self))
      (dns-service-process-result
       (operation-handle self)))))

(defun wait-for-result (operation &key (timeout *default-timeout*))
  (check-result
   (multiple-value-bind (object no-timeout-p)
       (mp:mailbox-read (operation-results-queue operation)
                        "Waiting for next operation result"
                        timeout)
     (if no-timeout-p
         object
       (error 'timeout-error)))))
