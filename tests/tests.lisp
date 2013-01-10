;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package #:com.wildora.dnssd-tests)

(defparameter *test-service-type* "_test._udp")
(defparameter *test-service-port* 9999)
(defconstant +test-timeout+ 2)

(defun operation-next-result-if (predicate operation)
  (loop :for result := (operation-next-result operation)
        :when (funcall predicate result)
        :do (return result)))

(defmacro signals (error &body body)
  `(handler-case (progn ,@body)
     (,error (condition)
       (declare (ignore condition))
       t)
     (condition (conndition)
       (declare (ignore condition))
       (error "Expected a condition of type ~A" ',error))
     (:no-error (&rest results)
       (declare (ignore results))
       (error "Failed to signal ~A" ',error))))

(defparameter *test-suite*
  '(test-if-name
    test-dispatch-run
    test-register-service
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
               (format *debug-io*
                       "~&Running ~A~%" test)
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
              (result-property result :presence))))

(defun test-dispatch-run ()
  (when (dispatcher-running-p)
    (dispatcher-stop))
  (assert (not (dispatcher-running-p)))
  (dispatcher-start)
  (signals error (dispatcher-start))
  (assert (dispatcher-running-p))
  (dispatcher-stop)
  (signals warning (dispatcher-stop)))

(defun test-register-service ()
  (let* ((operation (register *test-service-port*
                              *test-service-type*))
         (result (operation-next-result operation)))
    (test-presence result)
    ;; by the way test that properties signal errors when accessing an
    ;; unknown property.
    (signals error
      (result-property result :unknown-property))
    (cancel operation)))

(defun test-register-service-type-error ()
  (signals dnssd-error
    (register *test-service-port* "badtype")))

(defun do-registration-conflict (conflict-error-p)
  ;; 1. register a dummy service, automatically named
  (let* ((operation (register *test-service-port*
                              *test-service-type*
                              :name "Registration Conflict"
                              :no-auto-rename conflict-error-p))
         (result (operation-next-result operation))
         (registered-name (result-property result :name)))
    (test-presence result)
    ;; 2. register a service with the same name, but on a different
    ;; port for conflict
    (let* ((conflict-operation (register (1+ *test-service-port*)
                                         *test-service-type*
                                         :name registered-name
                                         :no-auto-rename t))
           (result (operation-next-result conflict-operation)))
      (test-presence result)
      (if conflict-error-p
          ;; 3a. conflict error: a condition is signaled when getting
          ;; the next result on the first operation.
          (signals dnssd-error
            (operation-next-result operation))
        ;; 3b. DNS-SD automatically renames our first service after
        ;; some time, by notifying on the first operation.
        (let ((result (operation-next-result operation)))
          (assert (eq (result-property result :presence) :remove))
          ;; 4. The service has eventually been renamed, compare
          ;; that the new name is different.
          (let ((result (operation-next-result operation)))
            (assert (eq (result-property result :presence) :add))
            (assert (not (string= registered-name
                              (result-property result :name)))))))
      (cancel conflict-operation))
    (cancel operation)))

(defun test-registration-conflict-1 ()
  (do-registration-conflict nil))

(defun test-registration-conflict-2 ()
  (do-registration-conflict t))

(defun test-registration-identical-service-timeout ()
  (let* ((operation (register *test-service-port*
                              *test-service-type*))
         (result (operation-next-result operation))
         (registered-name (result-property result :name)))
    (let* ((conflict-operation (register *test-service-port*
                                         *test-service-type*
                                         :name registered-name
                                         :no-auto-rename t)))
      ;; registering a service twice (with the same name, type and port) results
      ;; in a timeout.
      (signals result-timeout-error
        (operation-next-result conflict-operation
                               :timeout +test-timeout+))
      (cancel conflict-operation))
    (cancel operation)))

(defun test-enumerate-browse-domains ()
  (let* ((operation (enumerate-domains :domains :browse-domains))
         (result (operation-next-result operation)))
    (test-presence result)
    (cancel operation)))

(defun test-enumerate-registration-domains ()
  (let* ((operation (enumerate-domains :domains :registration-domains))
         (result (operation-next-result operation)))
    (test-presence result)
    (cancel operation)))

(defun match-service-presence (presence service-name)
  #'(lambda (result)
      (and (eq presence (result-property result :presence))
           (string= (result-property result :name)
                    service-name))))

(defun test-browse-timeout ()
  (let ((operation (browse "_inexistent_service_type._udp")))
    (signals result-timeout-error
      (operation-next-result operation :timeout +test-timeout+))
    (cancel operation)))

(defun test-browse ()
  (let* ((operation (register *test-service-port*
                              *test-service-type*
                              :name "Browse Test Service"))
         (result (operation-next-result operation))
         (registered-name (result-property result :name))
         (registered-domain (result-property result :domain))
         (registered-type (result-property result :type)))
    (test-presence result)
    (let* ((browse-operation (browse registered-type
                                     :domain registered-domain)))
      (operation-next-result-if (match-service-presence :add registered-name)
                                browse-operation)
      (cancel operation) ; called with no callback is blocking
      (operation-next-result-if (match-service-presence :remove registered-name)
                                browse-operation)
      (cancel browse-operation))))

(defun register-browse-and-resolve (service-name test-function)
  (let* ((register-operation (register *test-service-port*
                                       *test-service-type*
                                       :name service-name))
         (register-result (operation-next-result register-operation)))
    (let* ((browse-operation (browse (result-property register-result :type)
                                     :domain (result-property register-result :domain)))
           (browse-result (operation-next-result-if
                           (match-service-presence :add (result-property register-result :name))
                           browse-operation)))
      (let* ((resolve-operation (resolve (result-property browse-result :name)
                                         (result-property browse-result :type)
                                         (result-property browse-result :domain)))
             (resolve-result (operation-next-result resolve-operation)))
        (funcall test-function resolve-result)
        (cancel resolve-operation))
      (cancel browse-operation))
    (cancel register-operation)))

(defun test-resolve ()
  (register-browse-and-resolve
   "Resolve Test Service"
   #'(lambda (resolve-result)
       (assert (stringp (result-property resolve-result :full-name)))
       (assert (stringp (result-property resolve-result :host)))
       (assert (= *test-service-port*
                  (result-property resolve-result :port))))))

(defun test-get-addr-info ()
  (register-browse-and-resolve
   "GetAddrInfo Test Service"
   #'(lambda (resolve-result)
       (let* ((hostname (result-property resolve-result :host))
              (operation (get-addr-info hostname))
              (result (operation-next-result operation)))
          (assert (string= (result-property result :hostname)
                           hostname))
          (assert (stringp (result-property result :address)))))))

(defun test-nat-port-mapping-create ()
  (let* ((operation (nat-port-mapping-create 0))
         (result (operation-next-result operation)))
    (assert (stringp (result-property result :external-address)))
    (assert (zerop (result-property result :internal-port)))
    (assert (zerop (result-property result :external-port)))
    (assert (zerop (result-property result :ttl)))
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
  (let* ((record-type dnssd::+service-type-txt+)
         (record-class dnssd::+service-class-in+)
         (operation (register *test-service-port*
                              *test-service-type*))
         (result (operation-next-result operation)))
    (unwind-protect
        (progn
          (test-presence result)
          (let* ((record-data (build-txt-record *test-txt-properties*))
                 (record-ref (add-record operation record-type record-data)))
            (assert record-ref)
            (let* ((full-name (construct-full-name (result-property result :name)
                                                   *test-service-type*
                                                   "local"))
                   (operation (query-record full-name record-type record-class))
                   ;; For some reason DNS-SD returns two results, one
                   ;; with empty rdata and the other with our
                   ;; txt-record.
                   (result (operation-next-result-if #'(lambda (result)
                                                         (not (null (parse-txt-record
                                                          (result-property result :rdata)))))
                                                     operation)))
              (test-presence result)
              (assert (string= (result-property result :full-name)
                               full-name))
              (assert (= (result-property result :rrtype)
                         record-type))
              (assert (= (result-property result :rrclass)
                         record-class))
              (assert (equalp (result-property result :rdata)
                              record-data))
              (cancel operation))
            (update-record operation record-ref
                           (build-txt-record '(("foo.wildora.com" . "bar"))))
            (remove-record operation record-ref)))
        (cancel operation))))
