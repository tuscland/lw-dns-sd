;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package #:com.wildora.dnssd-tests)

(def-suite dnssd)
(in-suite dnssd)

(defparameter *test-service-type* "_test._udp")
(defparameter *test-service-port* 9999)
(defconstant +test-timeout+ 2)

(defmacro ensure-dispatch ()
  `(progn
     (when (dispatcher-running-p))))

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

(test merge-service
  (let* ((service (make-test-service))
         (merged (merge-service service :port 42)))
    (is (= 42 (service-port merged)))))

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
  (with-dispatcher
    (let* ((service (make-service :type "_test._udp." :port 9999))
           (operation (register service))
           (event (operation-wait-event operation)))
      (is-true (event-property event :success-p)))))

(test (register-service-type-error :depends-on dispatch-run)
  (with-dispatcher
    (signals dnssd-error
      (register (make-service :type "badtype")))))

(test (unknown-property :depends-on register-service)
  (with-dispatcher
    (let* ((service (make-service :type "_test._udp." :port 9999))
           (operation (register service))
           (event (operation-wait-event operation)))
      (signals dnssd-error
        (event-property event :unknown-property)))))

(defun do-registration-conflict (provoke-conflict-error)
  (with-dispatcher
    ;; 1. register a dummy service, automatically named
    (let* ((operation (register (make-test-service)
                                :no-auto-rename provoke-conflict-error))
           (event (operation-wait-event operation))
           (service (event-property event :service)))
      (is-true (event-property event :success-p))
      ;; 2. register a service with the same name, but on a different
      ;; port for conflict
      (let* ((conflict-service (merge-service service
                                              :port (1+ (service-port service))))
             (conflict-operation (register conflict-service :no-auto-rename t))
             (event (operation-wait-event conflict-operation)))
        (is-true (event-property event :success-p))
        ;; 3. Zeroconf will automatically rename our first service
        ;; after some time, by notifying on the first operation.
        (if provoke-conflict-error
            ;; This will signal a name-conflict-error
            (signals dnssd-error
              (operation-wait-event operation))
          ;; In the latter case, the original service will be renamed,
          ;; so we must handle the callbacks.
          (let ((event (operation-wait-event operation)))
            (is-false (event-property event :success-p))
            ;; 4. The service has eventually been renamed, compare
            ;; that the new name is different.
            (let ((event (operation-wait-event operation)))
              (is-true (event-property event :success-p))
              (is (not (string= (service-name service)
                                (service-name
                                 (event-property event :service))))))))))))

(test (registration-conflict-1 :depends-on register-service)
  (do-registration-conflict nil))

(test (registration-conflict-2 :depends-on register-service)
  (do-registration-conflict t))

(test (registration-identical-service-timeout :depends-on register-service)
  (with-dispatcher
    (let* ((service (make-test-service))
           (operation (register service))
           (event (operation-wait-event operation)))
    (let* ((conflict-service (event-property event :service))
           (conflict-operation (register conflict-service :no-auto-rename t)))
      ;; registering a service twice (with the same name, type and
      ;; port) results in a timeout.
      (signals event-timeout-error
        (operation-wait-event conflict-operation
                              :timeout +test-timeout+))))))

(test (enumerate-browse-domains :depends-on dispatch-run)
  (with-dispatcher
    (let* ((operation (enumerate-domains :domains :browse-domains))
           (event (operation-wait-event operation)))
      (is (eq :add (event-property event :presence))))))

(test (enumerate-registration-domains :depends-on dispatch-run)
  (with-dispatcher
    (let* ((operation (enumerate-domains :domains :registration-domains))
           (event (operation-wait-event operation)))
      (is (eq :add (event-property event :presence))))))

(defun match-service-presence (presence registered-service)
  #'(lambda (event)
      (and (eq presence (event-property event :presence))
           (string= (service-name
                     (event-property event :service))
                    (service-name registered-service)))))

(test (browse-timeout :depends-on register-service)
  (with-dispatcher
    (let ((operation (browse "_inexistent_service_type._udp")))
      (signals event-timeout-error
        (operation-wait-event operation :timeout +test-timeout+)))))

(test (browse :depends-on register-service)
  (with-dispatcher
    (let* ((operation (register (make-test-service "Browse Test Service")))
           (event (operation-wait-event operation))
           (service (event-property event :service)))
      (is-true (event-property event :success-p))
      (let* ((domain (make-domain :name (service-domain-name service)))
             (browse-operation (browse *test-service-type* :domain domain)))
        (finishes
          (operation-wait-event browse-operation
                                :test (match-service-presence :add service)))
        (cancel operation) ; called with no callback is blocking
        (finishes
          (operation-wait-event browse-operation
                                :test (match-service-presence :remove service)))))))

(defun register-browse-and-resolve (service-name test-function)
  (with-dispatcher
    (let* ((register-operation (register (make-test-service service-name)))
           (register-event (operation-wait-event register-operation))
           (registered-service (event-property register-event :service)))
      (let* ((domain (make-domain :name (service-domain-name registered-service)))
             (browse-operation (browse *test-service-type* :domain domain))
             (browse-event (operation-wait-event browse-operation
                                                 :test (match-service-presence :add registered-service))))
        (let* ((resolve-operation (resolve (event-property browse-event :service)))
               (resolve-event (operation-wait-event resolve-operation)))
          (funcall test-function registered-service resolve-event))))))

(test (resolve :depends-on browse)
  (register-browse-and-resolve
   "Resolve Test Service"
   #'(lambda (registered-service resolve-event)
       (let ((resolved-service (event-property resolve-event :service)))
        (is (string= (service-name resolved-service)
                     (service-name registered-service)))
        (is (string= (service-type resolved-service)
                     (service-type registered-service)))
        (is (string= (service-domain-name resolved-service)
                     (service-domain-name registered-service)))
        (is (stringp (service-host resolved-service)))
        (is (= (service-port registered-service)
               (service-port resolved-service)))))))

(test (get-addr-info :depends-on resolve)
  (register-browse-and-resolve
   "GetAddrInfo Test Service"
   #'(lambda (registered-service resolve-event)
       (declare (ignore registered-service))
       (let* ((resolved-service (event-property resolve-event :service))
              (hostname (service-host resolved-service))
              (operation (get-addr-info hostname))
              (event (operation-wait-event operation)))
          (is (string= (event-property event :hostname)
                       hostname))
          (is (stringp (event-property event :address)))))))

(test (nat-port-mapping-create :depends-on dispatch-run)
  (with-dispatcher
    (let* ((operation (nat-port-mapping-create 0))
           (event (operation-wait-event operation)))
      (is (stringp (event-property event :external-address)))
      (is (zerop (event-property event :internal-port)))
      (is (zerop (event-property event :external-port)))
      (is (zerop (event-property event :ttl))))))
