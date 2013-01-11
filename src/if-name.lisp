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

;;; Network interface name to index.


(in-package #:com.wildora.dns-sd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant IFNAMSIZ 16)

  #+win32
  (fli:register-module "Iphlpapi"))

(fli:define-foreign-function (if-name-to-index "if_nametoindex" :source)
    ((name (:reference-pass (:ef-mb-string :limit IFNAMSIZ))))
  :result-type (:unsigned :int)
  :language :ansi-c)

(fli:define-foreign-function (%if-index-to-name "if_indextoname" :source)
    ((index (:unsigned :int))
     (name  :pointer))
  :result-type :pointer
  :language :ansi-c)

(defun if-index-to-name (index)
  (fli:with-dynamic-foreign-objects ((name (:ef-mb-string :limit IFNAMSIZ)))
    (let ((result (%if-index-to-name index name)))
      (unless (fli:pointer-eq result fli:*null-pointer*)
        (fli:convert-from-foreign-string name)))))

(fli:define-c-struct (if-nameindex
                      (:foreign-name "if_nameindex"))
  (index (:unsigned :int))
  (name (:pointer :char)))

(fli:define-foreign-function (%if-nameindex "if_nameindex" :source)
    ()
  :result-type (:ptr (:struct if-nameindex))
  :language :ansi-c)

(fli:define-foreign-function (%if-freenameindex "if_freenameindex" :source)
    ((ptr (:pointer (:struct if-nameindex))))
  :result-type :void
  :language :ansi-c)

(defun if-name-index ()
  (let ((result (%if-nameindex)))
    (unless (fli:null-pointer-p result)
      (loop :with nameindex := (fli:copy-pointer result)
            :for index := (fli:foreign-slot-value nameindex 'index)
            :while (plusp index)
            :collect (cons index
                           (fli:convert-from-foreign-string
                            (fli:foreign-slot-value nameindex 'name)))
            :do (fli:incf-pointer nameindex)
            :finally (%if-freenameindex result)))))
