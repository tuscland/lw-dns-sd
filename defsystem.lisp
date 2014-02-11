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


(in-package "CL-USER")

(defsystem "COM.WILDORA.DNS-SD"
  (:default-pathname "src")
  :members ("package"
            "if-name"
            "constants"
            "errors"
            "txt-record"
            "result"
            "foreign"
            "fli-templates"
            "operation"
            "dispatch"
            "core"
            "api"
            "high-level")
  :rules ((:in-order-to :compile :all
           (:requires
            (:load "package")
            (:load "constants")))
          (:in-order-to :compile ("core" "fli-templates")
           (:requires
            (:load "foreign")))
          (:in-order-to :compile "operation"
           (:requires
            (:load "errors")
            (:load "result")))
          (:in-order-to :compile ("dispatch")
           (:requires
            (:load "errors")))
          (:in-order-to :compile ("api")
           (:requires
            (:load "foreign")
            (:load "dispatch")
            (:load "core")))))

(defsystem "COM.WILDORA.DNS-SD-TESTS"
  (:default-pathname "tests")
  :members (("COM.WILDORA.DNS-SD" :type :system)
            "package"
            "tests")
  :rules ((:in-order-to :compile :all
           (:requires  (:load :previous)))))
