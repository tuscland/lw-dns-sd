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

(defparameter *test-service-type* "_test._udp")
(defparameter *test-service-port* 9999)
(defconstant +test-timeout+ 2)

(defun wait-for-result-if (predicate operation)
  (loop :for result := (wait-for-result operation)
        :when (funcall predicate result)
        :do (return result)))

(defmacro signals (error &body body)
  `(with-simple-restart (continue "Continue testing")
     (handler-case (progn ,@body)
       (,error (condition)
         (declare (ignore condition))
         t)
       (condition (conndition)
         (declare (ignore condition))
         (error "Expected a condition of type ~A" ',error))
       (:no-error (&rest results)
         (error "Failed to signal ~A, instead got result: ~A"
                ',error results)))))

(defparameter *test-suite*
  '(test-if-name
    test-dispatch-run
    test-register-service
    test-cancel-operation-callback
    test-register-service-type-error
    test-registration-conflict-1
    test-registration-conflict-2
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
  (map nil #'(lambda (test)
               (format *debug-io* "~&~A~%" test)
               (funcall test))
       *test-suite*)
  t)

(defun test-if-name ()
  (let ((interfaces (if-name-index)))
    (assert (not (null interfaces)))
    (loop :for (index . name) :in interfaces :do
          (assert (string-equal name (if-index-to-name index)))
          (assert (= index (if-name-to-index name))))))

(defun test-presence (result &optional (presence :add))
  (assert (eq presence
              (result-value result :presence))))

(defun test-dispatch-run ()
  (when (dispatch-running-p)
    (stop-dispatch))
  (assert (not (dispatch-running-p)))
  (start-dispatch)
  (signals error (start-dispatch))
  (assert (dispatch-running-p))
  (stop-dispatch)
  (signals warning (stop-dispatch)))

(defun test-register-service ()
  (let* ((operation (register *test-service-port*
                              *test-service-type*))
         (result (wait-for-result operation)))
    (test-presence result)
    ;; also test unknown properties.
    (signals error
      (result-value result :unknown-property))
    (cancel operation)))

(defun test-cancel-operation-callback ()
  (let* ((operation (register *test-service-port*
                              *test-service-type*))
         cancel-result)
    (cancel operation :timeout 5 :callback (lambda ()
                                             (setf cancel-result :ok)))
    (unless (mp:process-wait-with-timeout nil 1 (lambda ()
                                                  cancel-result))
      (error "Timeout while testing cancel"))
    (assert (eq cancel-result :ok))))

(defun test-register-service-type-error ()
  (signals dns-sd-error
    (register *test-service-port* "badtype")))

(defun do-registration-conflict (conflict-error-p)
  (if (> (daemon-version) 3000000)
      ;; 1. register a dummy service, automatically named
      (let* ((operation (register *test-service-port*
                                  *test-service-type*
                                  :name "Registration Conflict"
                                  :no-auto-rename conflict-error-p))
             (result (wait-for-result operation))
             (registered-name (result-value result :name)))
        (test-presence result)
        ;; 2. register a service with the same name, but on a different
        ;; port for conflict
        (let* ((conflict-operation (register (1+ *test-service-port*)
                                             *test-service-type*
                                             :name registered-name
                                             :no-auto-rename t))
               (result (wait-for-result conflict-operation)))
          (test-presence result)
          (if conflict-error-p
              ;; 3a. conflict error: a condition is signaled when getting
              ;; the next result on the first operation.
              (signals dns-sd-error
                (wait-for-result operation))
            ;; 3b. DNS-SD automatically renames our first service after
            ;; some time, by notifying on the first operation.
            (let ((result (wait-for-result operation)))
              (assert (eq (result-value result :presence) :remove))
              ;; 4. The service has eventually been renamed, compare
              ;; that the new name is different.
              (let ((result (wait-for-result operation)))
                (assert (eq (result-value result :presence) :add))
                (assert (not (string= registered-name
                                      (result-value result :name)))))))
          (cancel conflict-operation))
        (cancel operation))
    (format t "~&Skipping test because daemon version is below 3000000~%")))

(defun test-registration-conflict-1 ()
  (do-registration-conflict nil))

(defun test-registration-conflict-2 ()
  (do-registration-conflict t))

(defun test-registration-identical-service-timeout ()
  (let* ((operation (register *test-service-port*
                              *test-service-type*))
         (result (wait-for-result operation))
         (registered-name (result-value result :name)))
    (let* ((conflict-operation (register *test-service-port*
                                         *test-service-type*
                                         :name registered-name
                                         :no-auto-rename t)))
      ;; registering a service twice (with the same name, type and port) results
      ;; in a timeout.
      (signals timeout-error
        (wait-for-result conflict-operation :timeout +test-timeout+))
      (cancel conflict-operation))
    (cancel operation)))

(defun test-enumerate-browse-domains ()
  (let* ((operation (enumerate-domains :domains :browse-domains))
         (result (wait-for-result operation)))
    (test-presence result)
    (cancel operation)))

(defun test-enumerate-registration-domains ()
  (let* ((operation (enumerate-domains :domains :registration-domains))
         (result (wait-for-result operation)))
    (test-presence result)
    (cancel operation)))

(defun match-service-presence (presence service-name)
  #'(lambda (result)
      (and (eq presence (result-value result :presence))
           (string= (result-value result :name)
                    service-name))))

(defun test-browse-timeout ()
  (let ((operation (browse "_inexistent_service_type._udp")))
    (signals timeout-error
      (wait-for-result operation :timeout +test-timeout+))
    (cancel operation)))

(defun test-browse ()
  (let* ((operation (register *test-service-port*
                              *test-service-type*
                              :name "Browse Test Service"))
         (result (wait-for-result operation))
         (registered-name (result-value result :name))
         (registered-domain (result-value result :domain))
         (registered-type (result-value result :type)))
    (test-presence result)
    (let* ((browse-operation (browse registered-type
                                     :domain registered-domain)))
      (wait-for-result-if (match-service-presence :add registered-name)
                          browse-operation)
      (cancel operation) ; called with no callback is blocking
      (wait-for-result-if (match-service-presence :remove registered-name)
                          browse-operation)
      (cancel browse-operation))))

(defun register-browse-and-resolve (service-name test-function)
  (let* ((register-operation (register *test-service-port*
                                       *test-service-type*
                                       :name service-name))
         (register-result (wait-for-result register-operation)))
    (let* ((browse-operation (browse (result-value register-result :type)
                                     :domain (result-value register-result :domain)))
           (browse-result (wait-for-result-if (match-service-presence :add (result-value register-result :name))
                                              browse-operation)))
      (let* ((resolve-operation (resolve (result-value browse-result :name)
                                         (result-value browse-result :type)
                                         (result-value browse-result :domain)))
             (resolve-result (wait-for-result resolve-operation)))
        (funcall test-function resolve-result)
        (cancel resolve-operation))
      (cancel browse-operation))
    (cancel register-operation)))

(defun test-resolve ()
  (register-browse-and-resolve
   "Resolve Test Service"
   #'(lambda (resolve-result)
       (assert (stringp (result-value resolve-result :full-name)))
       (assert (stringp (result-value resolve-result :hostname)))
       (assert (= *test-service-port*
                  (result-value resolve-result :port))))))

(defun test-get-addr-info ()
  (register-browse-and-resolve
   "GetAddrInfo Test Service"
   #'(lambda (resolve-result)
       (let* ((hostname (result-value resolve-result :hostname))
              (operation (get-addr-info hostname))
              (result (wait-for-result operation)))
          (assert (string= (result-value result :hostname)
                           hostname))
          (assert (stringp (result-value result :address)))
          (assert (result-value result :protocol))))))

(defun test-nat-port-mapping-create ()
  (let* ((operation (nat-port-mapping-create 0))
         (result (wait-for-result operation)))
    (assert (stringp (result-value result :external-address)))
    (assert (zerop (result-value result :internal-port)))
    (assert (zerop (result-value result :external-port)))
    (assert (zerop (result-value result :ttl)))
    (cancel operation)))

(defun test-create-connection ()
  (let ((operation (create-connection)))
    (cancel operation)))

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
  (let* ((record-type :TXT)
         (operation (register *test-service-port*
                              *test-service-type*))
         (result (wait-for-result operation)))
    (unwind-protect
        (progn
          (test-presence result)
          (let* ((record-data (build-txt-record *test-txt-properties*))
                 (record-ref (add-record operation record-type record-data)))
            (assert record-ref)
            (let* ((full-name (construct-full-name (result-value result :name)
                                                   *test-service-type*
                                                   "local"))
                   (operation (query-record full-name record-type))
                   ;; DNS-SD will return two results here, one with
                   ;; empty rdata and the other with our txt-record.
                   (result (wait-for-result-if #'(lambda (result)
                                                   (not (null (parse-txt-record
                                                               (result-value result :rdata)))))
                                               operation)))
              (test-presence result)
              (assert (string= (result-value result :full-name)
                               full-name))
              (assert (eq (result-value result :rrtype)
                          record-type))
              (assert (equalp (result-value result :rdata)
                              record-data))
              (cancel operation))
            (update-record operation record-ref
                           (build-txt-record '(("foo.wildora.com" . "bar"))))
            (remove-record operation record-ref)))
        (cancel operation))))
