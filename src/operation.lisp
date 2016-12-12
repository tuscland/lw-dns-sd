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
   (reply-mailbox
    :initform (mp:process-mailbox (mp:get-current-process))
    :initarg :reply-mailbox
    :reader operation-reply-mailbox)
   (reply-object
    :initform nil
    :initarg :reply-object))
  (:default-initargs
   :direction :input))

(defmethod initialize-instance :after ((self operation) &key handle)
  (setf (comm:socket-stream-socket self) (dns-service-sockfd handle)))

(defmethod cancel-operation ((self operation))
  (if (operation-canceled-p self)
      (warn "Operation ~A already canceled" self)
    (progn
      (dns-service-deallocate (operation-handle self))
      (setf (fli:pointer-address
             (operation-handle self)) 0))))

(defmethod operation-canceled-p ((self operation))
  (fli:null-pointer-p
   (operation-handle self)))

(defun make-event-object (reply-object result)
  (typecase reply-object
    (null result)
    (list (append reply-object (list result)))
    (t (list reply-object result))))

(defmethod enqueue-result ((self operation) class &rest initargs)
  (let ((reply-mailbox (slot-value self 'reply-mailbox)))
    (if reply-mailbox
        (mp:mailbox-send reply-mailbox
                         (make-event-object
                          (slot-value self 'reply-object)
                          (apply #'make-instance class initargs)))
      (warn "Operation ~A has no reply-mailbox" self))))


(declaim (special-dynamic *processed-operation*))

(defun reply (error-code more-coming-p &rest result-values)
  (check-error-code error-code)
  (enqueue-result *processed-operation* 'result
                  :more-coming-p more-coming-p
                  :values result-values))

(defmethod process-result ((self operation))
  "Called from the dispatcher to process pending results."
  (handler-bind ((result-error
                  #'(lambda (condition)
                      (enqueue-result self 'error-result
                                      :underlying-error condition))))
    (let ((*processed-operation* self))
      (dns-service-process-result
       (operation-handle self)))))
