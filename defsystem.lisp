;;;; -*- mode: LISP; syntax: COMMON-LISP; indent-tabs-mode: nil -*-

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


(in-package #:cl-user)

(defsystem #:dns-sd
  (:default-pathname "src")
  :members ("package"
            "if-name"
            "constants"
            "errors"
            "txt-record"
            "result"
            "foreign"
            "operation"
            "dispatcher"
            "core"
            "api")
  :rules ((:in-order-to :compile :all
           (:requires
            (:load "package")
            (:load "constants")))
          (:in-order-to :compile ("result")
           (:requires
            (:load "errors")))
          (:in-order-to :compile "operation"
           (:requires
            (:load "errors")
            (:load "result")))))

(defsystem #:dns-sd-tests
  (:default-pathname "tests")
  :members (("dns-sd" :type :system)
            "package"
            "tests")
  :rules ((:in-order-to :compile :all
           (:requires  (:load :previous)))))
