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

;;; Network interface name to index.


(in-package "COM.WILDORA.DNS-SD")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant IFNAMSIZ 16))

(fli:define-foreign-function (if-name-to-index "if_nametoindex")
    ((name (:reference-pass (:ef-mb-string :limit IFNAMSIZ))))
  :result-type (:unsigned :int)
  #+mswindows :module #+mswindows "iphlpapi")

(fli:define-foreign-function (%if-index-to-name "if_indextoname")
    ((index (:unsigned :int))
     (name  :pointer))
  :result-type :pointer
  #+mswindows :module #+mswindows "iphlpapi")

(defun if-index-to-name (index)
  (fli:with-dynamic-foreign-objects ((name (:ef-mb-string :limit IFNAMSIZ)))
    (let ((result (%if-index-to-name index name)))
      (unless (fli:pointer-eq result fli:*null-pointer*)
        (fli:convert-from-foreign-string name)))))

#-mswindows
(progn
  (fli:define-c-struct (if-nameindex
                        (:foreign-name "if_nameindex"))
    (index (:unsigned :int))
    (name (:pointer :char)))

  (fli:define-foreign-function (%if-nameindex "if_nameindex")
      ()
    :result-type (:ptr (:struct if-nameindex)))

  (fli:define-foreign-function (%if-freenameindex "if_freenameindex")
      ((ptr (:pointer (:struct if-nameindex))))
    :result-type :void)

  (defun if-name-index ()
    (let ((result (%if-nameindex)))
      (unless (fli:null-pointer-p result)
        (loop :for nameindex := (fli:copy-pointer result)
              :then (fli:incf-pointer nameindex)
              :for index := (fli:foreign-slot-value nameindex 'index)
              :while (plusp index)
              :collect (cons index
                             (fli:convert-from-foreign-string
                              (fli:foreign-slot-value nameindex 'name)))
              :finally (%if-freenameindex result))))))

#+mswindows
(progn
  (fli:define-foreign-function (%get-number-of-interfaces "GetNumberOfInterfaces")
      ((ptr (:pointer win32:dword)))
    :result-type win32:dword
    :module "iphlpapi")

  (defun get-number-of-interfaces ()
    (fli:with-dynamic-foreign-objects ((ptr win32:dword))
      (%get-number-of-interfaces ptr)
      (fli:dereference ptr)))

  (defun if-name-index ()
    (loop :for index :from 1 :to (get-number-of-interfaces)
          :collect (cons index
                         (if-index-to-name index)))))
