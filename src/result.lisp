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


(in-package "COM.WILDORA.DNS-SD")


(defclass base-result ()
  ((operation
    :reader result-operation
    :initform (error "OPERATION must be specified")
    :initarg :operation)
   (time
    :reader result-time
    :initform (get-universal-time))))


(defclass result (base-result)
  ((values
    :reader result-values
    :initform nil
    :initarg :values)
   (more-coming-p
    :reader result-more-coming-p
    :initform nil
    :initarg :more-coming-p)))

(defmethod result-value ((self result) (name symbol))
  ;; TODO: test value membership properly
  (unless (member name (result-values self))
    ;; TODO: make it a correctable error
    (error "Result ~A does have a value named ~A" self name))
  (getf (result-values self)
        name))

(defmethod print-object ((self result) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "MORE-COMING: ~A, ~S"
            (result-more-coming-p self)
            (result-values self))))

(defclass error-result (base-result)
  ((underlying-error
    :reader error-result-underlying-error
    :initarg :underlying-error
    :initform (error "UNDERLYING-ERROR must be specified"))))
