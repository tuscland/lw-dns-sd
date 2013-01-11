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

;;; Event queue specialized for DNS-SD events.


(in-package #:com.wildora.dns-sd)

(defglobal-variable *operations* nil)
(defglobal-variable *process* nil)
(defglobal-variable *cancel-operation-lock* (mp:make-lock :sharing t))
(defconstant *process-join-timeout* 10)


(defun dispatcher-running-p ()
  (mp:process-alive-p *process*))

(defun dispatcher-start ()
  (when (dispatcher-running-p)
    (error "DNS-SD Dispatcher is already started."))

  #+win32 (fli:register-module "dnssd")
  (assert (> (daemon-version) 0))

  (setf *process*
        (mp:process-run-function "DNS-SD Dispatcher"
                                 '(:mailbox t)
                                 #'dispatcher-loop))
  (values))

(defun dispatcher-stop ()
  (if (dispatcher-running-p)
      (progn
        (dispatcher-send #'(lambda ()
                             (mp:process-kill
                              (mp:get-current-process))))
        (mp:process-join *process*
                         :timeout *process-join-timeout*)
        (setf *process* nil))
    (warn "DNS-SD Dispatcher not running"))  
  (values))

(defun %add-operation (operation)
  (push operation *operations*))

(defun %remove-operation (operation &optional (removal-callback #'do-nothing))
  (setf *operations*
        (remove operation *operations*))
  (cancel-operation operation)
  (funcall removal-callback))

(defun %cleanup (process)
  (declare (ignore process))
  (dolist (operation (copy-seq *operations*))
    (%remove-operation operation)))

(defun dispatcher-wait-reason ()
  (format nil "Waiting, ~[no~:;~:*~D~] pending operation~:P"
          (length *operations*)))

(defun dispatcher-loop ()
  (mp:ensure-process-cleanup `%cleanup)
  (loop :with mailbox := (mp:process-mailbox (mp:get-current-process))
        :for operation := (sys:wait-for-input-streams-returning-first
                           *operations*
                           :wait-reason (dispatcher-wait-reason)
                           :wait-function #'(lambda ()
                                              #+lispworks6.1 (mp:mailbox-not-empty-p mailbox)
                                              #-lispworks6.1 (mp:mailbox-peek mailbox)))
        :do (with-simple-restart (abort "Return to event loop.")
              (if operation
                  (handler-case (process-result operation)
                    (result-error (condition)
                      (declare (ignore condition))
                      (%remove-operation operation)))
                (mp:process-all-events)))
        :while t))

(defun dispatcher-send (form)
  (when (not (dispatcher-running-p))
    (dispatcher-start))
  (mp:process-send *process* form)
  (mp:process-poke *process*))

(defun dispatcher-add-operation (operation)
  (dispatcher-send
   `(%add-operation ,operation))
  operation)

(defun dispatcher-remove-operation (operation removal-callback)
  (dispatcher-send
   `(%remove-operation ,operation ,removal-callback)))


;;;;
;;;; Public interface
;;;;

(define-condition cancel-timeout-error (dns-sd-error)
  ())

(defparameter *default-cancel-timeout* 60)

(defun cancel (operation
               &key callback
                    (timeout *default-cancel-timeout*))
  (let (finished-waiting)
    (dispatcher-remove-operation operation
                                 (or callback
                                     #'(lambda ()
                                         (setf finished-waiting t))))
    (when (not (null callback))
      (unless (mp:process-wait-with-timeout "Waiting for operation to be canceled."
                                            timeout
                                            #'(lambda ()
                                                finished-waiting))
        (error 'cancel-timeout-error))))
  (values))

(defun dispatch (&rest operation-initargs)
  (dispatcher-add-operation
   (apply #'make-instance 'operation
          operation-initargs)))
