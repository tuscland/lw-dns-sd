;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package #:com.wildora.dnssd-tests)

(in-suite dnssd)

(defparameter *test-service-type* "_test._udp")
(defparameter *test-service-port* 9999)

(defun make-test-service (&optional name)
  (make-service :name name :type *test-service-type* :port *test-service-port*))

(test if-name
  (let ((interfaces (if-name-index)))
    (is (not (null interfaces)))
    (loop :for (index . name) :in interfaces
          :do
          (is (string-equal name (if-index-to-name index)))
          (is (= index (if-name-to-index name))))))

(test dnssd-error
  (signals dnssd-error (dnssd-error -65537)))

(test service-predicate
  (is (service-p (make-test-service))))

(test service-equal
  (let ((service1 (make-test-service))
        (service2 (make-test-service))
        (service3 (make-service :interface-index 1 :type "_test._udp" :port 9999)))
    (is (service-equal service1 service2))
    (is-false (service-equal service1 service3))))

(test dispatch-run
  (when (running-p)
    (stop))
  (is-false (running-p))
  (start)
  (signals error (start))
  (is (running-p))
  (stop)
  (signals warning (stop)))

(defconstant +test-timeout+ 2)
(defconstant +test-long-timeout+ 10)

(test merge-service
  (let* ((service (make-test-service))
         (merged (merge-service service :port 42)))
    (is (= 42 (service-port merged)))))

(test (register-service :depends-on dispatch-run)
  (let* ((service (make-service :type "_test._udp." :port 9999))
         (operation (register service)))
    (let ((result (operation-wait-result operation :timeout +test-timeout+)))
      (is-true (result-property result :success-p)))
      (is-true (cancel operation))))

(test (register-service-type-error :depends-on dispatch-run)
  (signals dnssd-error
    (register (make-service :type "badtype"))))

(test (unknown-property :depends-on register-service)
  (let* ((service (make-service :type "_test._udp." :port 9999))
         (operation (register service)))
    (let ((result (operation-wait-result operation :timeout +test-timeout+)))
      (signals error (result-property result :unknown-property)))
      (is-true (cancel operation))))

(test (registration-conflict :depends-on register-service)
  (stop)
  ;; 1. register a dummy service, automatically named
  (let* ((operation (register (make-test-service)))
         (result (operation-wait-result operation :timeout +test-timeout+))
         (service (result-property result :service))
         (name (service-name service)))
    (is-true (result-property result :success-p))
    ;; 2. register a service with the same name, but on a different
    ;; port for conflict
    (let ((conflict-service (merge-service service
                                           :port (1+ (service-port service)))))
      (let* ((conflict-operation (register conflict-service :no-auto-rename t))
             (result (operation-wait-result conflict-operation
                                            :timeout +test-timeout+)))
        (is-true (result-property result :success-p))
        ;; 3. Zeroconf will automatically rename our first service
        ;; after some time.
        (let ((result (operation-wait-result operation
                                             :timeout +test-long-timeout+)))
          (is-false (result-property result :success-p))
          ;; 4. The service has finally been renamed, compare that the
          ;; new name is different.
          (let ((result (operation-wait-result operation
                                               :timeout +test-long-timeout+)))
            (is-true (result-property result :success-p))
            (is (not (string= name
                              (service-name
                               (result-property result :service)))))))
        (is-true (cancel conflict-operation))))
    (is-true (cancel operation))))

(test (registration-identical-service-timeout :depends-on registration-conflict)
  (let* ((service (make-test-service))
         (operation (register service))
         (result (operation-wait-result operation :timeout +test-timeout+)))
    (let* ((conflict-service (copy-service (result-property result :service)))
           (conflict-operation (register conflict-service :no-auto-rename t)))
      (signals operation-timeout-error
        (operation-wait-result conflict-operation
                               :timeout +test-timeout+))
      (is-true (cancel conflict-operation)))
    (is-true (cancel operation))))

(test (enumerate-browse-domains :depends-on register-service)
  (let* ((operation (enumerate-domains :domains :browse-domains))
         (result (operation-wait-result operation :timeout +test-timeout+)))
    (is (eq :add (result-property result :presence)))
    (is-true (cancel operation))))

(test (enumerate-registration-domains :depends-on register-service)
  (let* ((operation (enumerate-domains :domains :registration-domains))
         (result (operation-wait-result operation :timeout +test-timeout+)))
    (is (eq :add (result-property result :presence)))
    (is-true (cancel operation))))

(defun make-browse-test (presence registered-service)
  #'(lambda (result)
      (and (eq presence (result-property result :presence))
           (string= (service-name
                     (result-property result :service))
                    (service-name registered-service)))))

(test (browse :depends-on enumerate-browse-domains)
  (stop)
  (let* ((register-operation (register (make-test-service "Browse Test Service")))
         (register-result (operation-wait-result register-operation :timeout +test-timeout+))
         (registered-service (result-property register-result :service)))
    (is-true (result-property register-result :success-p))
    (let* ((browse-operation
            (browse *test-service-type*
                    :domain (make-domain :name (service-domain-name registered-service))))
           (browse-result (operation-wait-result browse-operation
                                                 :test (make-browse-test :add registered-service)
                                                 :timeout +test-timeout+)))
      (is (not (null browse-result)))
      (is-true (cancel register-operation))
      (mp:process-wait "Waiting for registered operation to be cancelled."
                       #'(lambda ()
                           (operation-cancelled-p register-operation)))
      (setf browse-result (operation-wait-result browse-operation
                                                 :test (make-browse-test :remove registered-service)
                                                 :timeout +test-timeout+))
      (is (not (null browse-result)))
      (is-true (cancel browse-operation)))))

(test (browse-timeout :depends-on browse)
  (stop)
  (let ((operation (browse "_inexistent_service_type._udp")))
    (signals operation-timeout-error
      (operation-wait-result operation :timeout +test-timeout+))
    (cancel operation)))

(test (resolve :depends-on browse)
  (stop)
  (let* ((register-operation (register (make-test-service "Resolve Test Service")))
         (register-result (operation-wait-result register-operation :timeout +test-timeout+))
         (registered-service (result-property register-result :service)))
    (let* ((browse-operation
            (browse *test-service-type*
                    :domain (make-domain :name (service-domain-name registered-service))))
           (browse-result (operation-wait-result browse-operation
                                                 :test (make-browse-test :add registered-service)
                                                 :timeout +test-timeout+)))
      (let* ((resolve-operation (resolve (result-property browse-result :service)))
             (resolve-result (operation-wait-result resolve-operation :timeout +test-timeout+))
             (resolved-service (result-property resolve-result :service)))
        (is (string= (service-name resolved-service)
                     (service-name registered-service)))
        (is (string= (service-type resolved-service)
                     (service-type registered-service)))
        (is (string= (service-domain-name resolved-service)
                     (service-domain-name registered-service)))
        (is (stringp (service-host resolved-service)))
        (is (= (service-port registered-service)
               (service-port resolved-service)))
        (is-false (cancel resolve-operation)))
      (is-true (cancel browse-operation)))
    (is-true (cancel register-operation))))

(test (get-addr-info :depends-on resolve)
  (stop)
  (let* ((register-operation (register (make-test-service "GetAddrInfo Test Service")))
         (register-result (operation-wait-result register-operation :timeout +test-timeout+))
         (registered-service (result-property register-result :service)))
    (let* ((browse-operation
            (browse *test-service-type*
                    :domain (make-domain :name (service-domain-name registered-service))))
           (browse-result (operation-wait-result browse-operation
                                                 :test (make-browse-test :add registered-service)
                                                 :timeout +test-timeout+)))
      (let* ((resolve-operation (resolve (result-property browse-result :service)))
             (resolve-result (operation-wait-result resolve-operation :timeout +test-timeout+))
             (resolved-service (result-property resolve-result :service)))
        (let* ((hostname (service-host resolved-service))
               (get-addr-info-operation (get-addr-info hostname))
               (get-addr-info-result (operation-wait-result get-addr-info-operation :timeout +test-timeout+)))
          (is (string= (result-property get-addr-info-result :hostname)
                       hostname))
          (is (stringp (result-property get-addr-info-result :address)))
          (is-false (cancel get-addr-info-operation)))
        (is-false (cancel resolve-operation)))
      (is-true (cancel browse-operation)))
    (is-true (cancel register-operation))))

(test (nat-port-mapping-create :depends-on dispatch-run)
  (let* ((operation (nat-port-mapping-create 0)))
    (let ((result (operation-wait-result operation :timeout +test-timeout+)))
      (is (stringp (result-property result :external-address)))
      (is (zerop (result-property result :internal-port)))
      (is (zerop (result-property result :external-port)))
      (is (zerop (result-property result :ttl))))
    (is-false (cancel operation))))
