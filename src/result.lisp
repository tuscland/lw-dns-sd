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

;;; Results are events produced by operations.  They are delivered to
;;; asynchronous operations callbacks or returned from the synchronous
;;; call OPERATION-NEXT-RESULT.


(in-package #:com.wildora.dns-sd)

(defclass result ()
  ())

(defgeneric result-value (result value-name)
  (:method ((result result) (value-name symbol))
   (error "Result ~A does not support named values" result)))

(defgeneric result-more-coming-p (result)
  (:method ((result result))
   nil))

(defgeneric check-result (result)
  (:method ((result result))
   result))

(defclass reply-result (result)
  ((values
    :reader result-values
    :initform nil
    :initarg :properties)
   (more-coming-p
    :reader result-more-coming-p
    :initform nil
    :initarg :more-coming-p)))

(defmethod result-value ((self reply-result) (name symbol))
  ;; TODO: test property membership properly
  (unless (member name (result-values self))
    (error "Result ~A does have a value named ~A" self name))
  (getf (result-values self)
        name))

(defmethod print-object ((self reply-result) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "MORE-COMING: ~A, ~S"
            (result-more-coming-p self)
            (result-values self))))


(defclass error-result (result)
  ((error
    :reader error-result-error
    :initform nil
    :initarg :error)))

(defmethod check-result ((self error-result))
  (error (error-result-error self)))

(defun make-error-result (error)
  (make-instance 'error-result :error error))

(defun make-result (more-coming-p properties)
  (make-instance 'reply-result
                 :more-coming-p more-coming-p
                 :properties properties))
