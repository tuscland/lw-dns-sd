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

;;; Test suite for the COM.WILDORA.DNS-SD system.


(in-package "COM.WILDORA.DNS-SD-TESTS")


(defgeneric check-result (object))

(defmethod check-result ((object result))
  object)

(defmethod check-result ((object error-result))
  (error (error-result-underlying-error object)))


(defun wait-for-result (operation &key (timeout *default-timeout*))
  (check-result
   (multiple-value-bind (result no-timeout-p)
       (mp:mailbox-read (operation-reply-mailbox operation)
                        "Waiting for next DNS-SD result"
                        timeout)
     (if no-timeout-p
         result
       (error 'timeout-error)))))

(defun wait-for-result-until (operation predicate)
  "Wait for a result satistfying PREDICATE to occur on OPERATION.
Predicate is a function taking one argument RESULT."
  (loop :for result := (wait-for-result operation)
        :when (funcall predicate result)
        :return result))

(defmacro bind-result-values ((&rest symbols) result &body body)
  (if (null symbols)
      `(progn ,@body)
    (let ((result-var (gensym "RESULT")))
      (flet ((result-binding (symbol)
               (etypecase symbol
                 (symbol
                  (let ((keyword (intern (string symbol) "KEYWORD")))
                    `(,symbol
                      (result-value ,result-var ,keyword))))
                 (list
                  `(,(first symbol)
                    (result-value ,result-var ,(second symbol)))))))
        `(let* ((,result-var ,result)
                ,@(mapcar #'result-binding symbols))
           ,@body)))))

(defmacro run-operation ((operation operation-form) &body body)
  "Ensures CANCEL is called on OPERATION after BODY is executed."
  `(let ((,operation ,(append operation-form
                              '(:reply-mailbox (mp:make-mailbox)))))
       (unwind-protect
           (progn ,@body)
         (cancel ,operation))))


(defmacro signals (error-type &body body)
  `(with-simple-restart (continue "Continue testing")
     (handler-case (progn ,@body)
       (,error-type (condition)
         (declare (ignore condition))
         t)
       (condition (condition)
         (declare (ignore condition))
         (error "Unexpected Error: \"~A\"" ',error-type condition))
       (:no-error (&rest results)
         (error "Failed to signal ~A, instead got result: ~A"
                ',error-type results)))))

(defparameter *test-suite*
  '(test-if-name
    test-dispatch-run
    test-register-service
    test-cancel-operation-callback
    test-register-service-type-error
    test-registration-identical-service-timeout
    test-enumerate-browse-domains
    test-enumerate-registration-domains
    test-browse-timeout
    test-browse
    test-resolve
    test-get-addr-info
    test-nat-port-mapping-create
    test-create-connection
    test-construct-full-name
    test-txt-record
    test-record-crud))

(defun run-tests ()
  (map nil (lambda (test)
             (format *debug-io* "~&~A~%" test)
             (funcall test))
       *test-suite*)
  t)

(defparameter *test-service-type* "_test._udp")
(defparameter *test-service-port* 9999)
(defconstant +test-timeout+ 2)

(defun test-if-name ()
  (let ((interfaces (if-name-index)))
    (assert (not (null interfaces)))
    (loop :for (index . name) :in interfaces :do
          (assert (string-equal name (if-index-to-name index)))
          (assert (= index (if-name-to-index name))))))

(defun test-dispatch-run ()
  (when (dispatch-running-p)
    (stop-dispatch))
  (assert (not (dispatch-running-p)))
  (start-dispatch)
  (signals error (start-dispatch))
  (assert (dispatch-running-p))
  (stop-dispatch)
  (signals warning (stop-dispatch)))

(defmacro with-next-result (operation &body body)
  (let ((result (gensym "RESULT")))
    `(let ((,result (wait-for-result ,operation)))
       (macrolet ((rv (flag)
                    `(result-value ,',result ,flag)))
         ,@body))))

(defun test-register-service ()
  (run-operation (operation (register *test-service-port* *test-service-type*))
    (with-next-result operation
      (assert (eq (rv :presence) :add))
      ;; also test unknown properties.
      (signals error
        (rv :unknown-property)))))

(defun test-cancel-operation-callback ()
  (let ((operation (register *test-service-port* *test-service-type*
                             :reply-mailbox (mp:make-mailbox)))
        (barrier (mp:make-barrier 2)))
    (wait-for-result operation)
    (cancel operation :timeout 5 :callback `(mp:barrier-wait ,barrier))
    (case (mp:barrier-wait barrier :timeout 1)
      (:timeout (error "Timeout while testing cancel")))))

(defun test-register-service-type-error ()
  (signals dns-sd-error
    (register *test-service-port* "badtype")))

(defun test-registration-identical-service-timeout ()
  (run-operation (operation (register *test-service-port* *test-service-type*))
    (with-next-result operation
      (run-operation (conflict-operation (register *test-service-port*
                                                   *test-service-type*
                                                   :name (rv :name)
                                                   :no-auto-rename t))
        ;; registering a service twice (with the same name, type and
        ;; port) results in a timeout.
        (signals timeout-error
          (wait-for-result conflict-operation
                           :timeout +test-timeout+))))))

(defun test-enumerate-browse-domains ()
  (run-operation (operation (enumerate-domains :domains :browse-domains))
    (with-next-result operation
      (assert (eq (rv :presence) :add)))))

(defun test-enumerate-registration-domains ()
  (run-operation (operation (enumerate-domains :domains :registration-domains))
    (with-next-result operation
      (assert (eq (rv :presence) :add)))))

(defun service-presence-matcher (presence service-name)
  (lambda (result)
    (and (eq presence (result-value result :presence))
         (string= (result-value result :name)
                  service-name))))

(defun test-browse-timeout ()
  (run-operation (operation (browse "_inexistent_service_type._udp"))
    (signals timeout-error
      (wait-for-result operation :timeout +test-timeout+))))

(defun test-browse ()
  (run-operation (operation (register *test-service-port*
                                      *test-service-type*
                                      :name "Browse Test Service"))
    (bind-result-values (name domain type presence)
        (wait-for-result operation)
      (declare (ignore presence))
      (run-operation (browse-operation (browse type :domain domain))
        (wait-for-result-until browse-operation (service-presence-matcher :add name))
        (cancel operation)
        (wait-for-result-until browse-operation (service-presence-matcher :remove name))))))

(defun register-browse-and-resolve (service-name test-function)
  (run-operation (register-operation (register *test-service-port*
                                               *test-service-type*
                                               :name service-name))
    (bind-result-values (name type domain)
        (wait-for-result register-operation)
      (run-operation (browse-operation (browse type :domain domain))
        (bind-result-values (name type domain)
            (wait-for-result-until browse-operation (service-presence-matcher :add name))
          (run-operation (resolve-operation (resolve name type domain))
            (funcall test-function
                     (wait-for-result resolve-operation))))))))

(defun test-resolve ()
  (register-browse-and-resolve
   "Resolve Test Service"
   (lambda (result)
     (bind-result-values (full-name hostname port) result
       (assert (stringp full-name))
       (assert (stringp hostname))
       (assert (= *test-service-port* port))))))

(defun test-get-addr-info ()
  (register-browse-and-resolve
   "GetAddrInfo Test Service"
   (lambda (result)
     (bind-result-values ((resolved-hostname :hostname)) result
       (run-operation (operation (get-addr-info resolved-hostname))
         (bind-result-values (hostname address protocol)
             (wait-for-result operation)
           (assert (string= hostname resolved-hostname))
           (assert (stringp address))
           (assert protocol)))))))

(defun test-nat-port-mapping-create ()
  (run-operation (operation (nat-port-mapping-create 0))
    (bind-result-values (external-address internal-port external-port ttl)
        (wait-for-result operation)
      (assert (stringp external-address))
      (assert (zerop internal-port))
      (assert (zerop external-port))
      (assert (zerop ttl)))))

(defun test-create-connection ()
  (run-operation (operation (create-connection))
    (values)))

(defun test-construct-full-name ()
  (let ((actual (construct-full-name "foo service" "_bar._udp" "local"))
        (expected "foo\\032service._bar._udp.local."))
    (assert (string= actual expected)))
  (let ((actual (construct-full-name nil "_bar._udp" "local"))
        (expected "_bar._udp.local."))
    (assert (string= actual expected))))

(defvar *test-txt-properties*
  '(("key1.wildora.com" . "value1")
    ("key2.wildora.com" . "value2")
    ("eat-glass.wildora.com" . "Ek get etið gler án þess að verða sár")))

(defun test-txt-record ()
  (let ((properties *test-txt-properties*))
    (assert (equalp (parse-txt-record
                     (build-txt-record properties))
                    properties))))

(defun test-record-crud ()
  (run-operation (operation (register *test-service-port*
                                      *test-service-type*))
    (bind-result-values ((register-name :name) presence)
        (wait-for-result operation)
      (assert (eq presence :add))
      (let* ((record-type :TXT)
             (record-data (build-txt-record *test-txt-properties*))
             (record-ref (add-record operation record-type record-data))
             (our-full-name (construct-full-name register-name
                                                 *test-service-type*
                                                 "local")))
        (assert record-ref)
        (run-operation (operation (query-record our-full-name record-type))
          (bind-result-values (full-name rrtype rdata presence)
              (wait-for-result-until operation
                                     (lambda (result)
                                       (not (null (parse-txt-record
                                                   (result-value result :rdata))))))
            ;; DNS-SD will return two results here, one with empty
            ;; rdata and the other with our txt-record.
            (assert (eq presence :add))
            (assert (string= full-name our-full-name))
            (assert (eq rrtype record-type))
            (assert (equalp rdata record-data))))
        (update-record operation record-ref
                       (build-txt-record '(("foo.wildora.com" . "bar"))))
        (remove-record operation record-ref)))))
