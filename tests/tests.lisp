;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package #:com.wildora.dnssd-tests)

(def-suite dnssd)
(in-suite dnssd)

(defparameter *test-service-type* "_test._udp")
(defparameter *test-service-port* 9999)
(defconstant +test-timeout+ 2)

(defun operation-next-result-if (predicate operation)
  (loop :for result := (operation-next-result operation)
        :when (funcall predicate result)
        :do (return result)))

(test if-name
  (let ((interfaces (if-name-index)))
    (is (not (null interfaces)))
    (loop :for (index . name) :in interfaces
          :do
          (is (string-equal name (if-index-to-name index)))
          (is (= index (if-name-to-index name))))))

(test dispatch-run
  (when (dispatcher-running-p)
    (dispatcher-stop))
  (is-false (dispatcher-running-p))
  (dispatcher-start)
  (signals error (dispatcher-start))
  (is (dispatcher-running-p))
  (dispatcher-stop)
  (signals warning (dispatcher-stop)))

(test (register-service :depends-on dispatch-run)
  (let* ((operation (register *test-service-port*
                              *test-service-type*))
         (result (operation-next-result operation)))
    (is (eq (result-property result :presence) :add))
    (cancel operation)))

(test (register-service-type-error :depends-on dispatch-run)
  (signals dnssd-error
    (register *test-service-port* "badtype")))

(test (unknown-property :depends-on register-service)
  (let* ((operation (register *test-service-port*
                              *test-service-type*))
         (result (operation-next-result operation)))
    (signals error
      (result-property result :unknown-property))
    (cancel operation)))

(defun do-registration-conflict (conflict-error-p)
  ;; 1. register a dummy service, automatically named
  (let* ((operation (register *test-service-port*
                              *test-service-type*
                              :name "Registration Conflict"
                              :no-auto-rename conflict-error-p))
         (result (operation-next-result operation))
         (registered-name (result-property result :name)))
    (is (eq (result-property result :presence) :add))
    ;; 2. register a service with the same name, but on a different
    ;; port for conflict
    (let* ((conflict-operation (register (1+ *test-service-port*)
                                         *test-service-type*
                                         :name registered-name
                                         :no-auto-rename t))
           (result (operation-next-result conflict-operation)))
      (is (eq (result-property result :presence) :add))
      (if conflict-error-p
          ;; 3a. conflict error: a condition is signaled when getting
          ;; the next result on the first operation.
          (signals dnssd-error
            (operation-next-result operation))
        ;; 3b. DNS-SD automatically renames our first service after
        ;; some time, by notifying on the first operation.
        (let ((result (operation-next-result operation)))
          (is (eq (result-property result :presence) :remove))
          ;; 4. The service has eventually been renamed, compare
          ;; that the new name is different.
          (let ((result (operation-next-result operation)))
            (is (eq (result-property result :presence) :add))
            (is (not (string= registered-name
                              (result-property result :name)))))))
      (cancel conflict-operation))
    (cancel operation)))

(test (registration-conflict-1 :depends-on register-service)
  (do-registration-conflict nil))

(test (registration-conflict-2 :depends-on register-service)
  (do-registration-conflict t))

(test (registration-identical-service-timeout :depends-on register-service)
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

(test (enumerate-browse-domains :depends-on dispatch-run)
  (let* ((operation (enumerate-domains :domains :browse-domains))
         (result (operation-next-result operation)))
    (is (eq :add (result-property result :presence)))
    (cancel operation)))

(test (enumerate-registration-domains :depends-on dispatch-run)
  (let* ((operation (enumerate-domains :domains :registration-domains))
         (result (operation-next-result operation)))
    (is (eq :add (result-property result :presence)))
    (cancel operation)))

(defun match-service-presence (presence service-name)
  #'(lambda (result)
      (and (eq presence (result-property result :presence))
           (string= (result-property result :name)
                    service-name))))

(test (browse-timeout :depends-on register-service)
  (let ((operation (browse "_inexistent_service_type._udp")))
    (signals result-timeout-error
      (operation-next-result operation :timeout +test-timeout+))
    (cancel operation)))

(test (browse :depends-on register-service)
  (let* ((operation (register *test-service-port*
                              *test-service-type*
                              :name "Browse Test Service"))
         (result (operation-next-result operation))
         (registered-name (result-property result :name))
         (registered-domain (result-property result :domain))
         (registered-type (result-property result :type)))
    (is (eq (result-property result :presence) :add))
    (let* ((browse-operation (browse registered-type
                                     :domain registered-domain)))
      (finishes
        (operation-next-result-if (match-service-presence :add registered-name)
                                  browse-operation))
      (cancel operation) ; called with no callback is blocking
      (finishes
        (operation-next-result-if (match-service-presence :remove registered-name)
                                  browse-operation))
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

(test (resolve :depends-on browse)
  (register-browse-and-resolve
   "Resolve Test Service"
   #'(lambda (resolve-result)
       (is (stringp (result-property resolve-result :full-name)))
       (is (stringp (result-property resolve-result :host)))
       (is (= *test-service-port*
              (result-property resolve-result :port))))))

(test (get-addr-info :depends-on resolve)
  (register-browse-and-resolve
   "GetAddrInfo Test Service"
   #'(lambda (resolve-result)
       (let* ((hostname (result-property resolve-result :host))
              (operation (get-addr-info hostname))
              (result (operation-next-result operation)))
          (is (string= (result-property result :hostname)
                       hostname))
          (is (stringp (result-property result :address)))))))

(test (nat-port-mapping-create :depends-on dispatch-run)
  (let* ((operation (nat-port-mapping-create 0))
         (result (operation-next-result operation)))
    (is (stringp (result-property result :external-address)))
    (is (zerop (result-property result :internal-port)))
    (is (zerop (result-property result :external-port)))
    (is (zerop (result-property result :ttl)))
    (cancel operation)))

(test (create-connection :depends-on dispatch-run)
  (finishes
    (let ((operation (create-connection)))
      (cancel operation))))
