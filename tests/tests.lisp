;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package #:zeroconf-tests)

(in-suite zeroconf)

(defparameter *test-service-type* "_test._udp")
(defparameter *test-service-port* 9999)

(defun make-test-service ()
  (make-service :type *test-service-type* :port *test-service-port*))

(defparameter *test-service* (make-test-service))

(test if-name
  (let ((interfaces (if-name-index)))
    (is (not (null interfaces)))
    (loop :for (index . name) :in interfaces
          :do
          (is (string-equal name (if-index-to-name index)))
          (is (= index (if-name-to-index name))))))

(test zeroconf-error
  (signals zeroconf-error (zeroconf-error -65537)))

(test service-predicate
  (is (service-p *test-service*)))

(test service-equal
  (let ((service1 (make-test-service))
        (service2 (make-test-service))
        (service3 (make-service :interface-index 1 :type "_test._udp" :port 9999)))
    (is (service-equal service1 service2))
    (is-false (service-equal service1 service3))))

(test (zeroconf-version :depends-on if-name)
  (is (plusp (daemon-version))))

(test (dispatch-run :depends-on zeroconf-version)
  (when (running-p)
    (stop))
  (is-false (running-p))
  (start)
  (signals zeroconf-error (start))
  (is (running-p))
  (stop)
  (signals warning (stop)))

(defconstant +test-timeout+ 2)
(defconstant +test-long-timeout+ 10)

(test (register-service :depends-on dispatch-run)
  (let* ((service (make-service :type "_test._udp." :port 9999))
         (operation (register service)))
    (let ((result (operation-next-result operation)))
      (is (register-result-p result))
      (is-false (register-result-conflict-p result))
      (is-true (cancel operation)))))

(test (register-service-type-error :depends-on dispatch-run)
  (signals zeroconf-error
    (register (make-service :type "badtype"))))

(test (registration-conflict :depends-on register-service)
  (stop)
  ;; 1. register a dummy service, automatically named
  (let* ((service *test-service*)
         (operation (register service))
         (result (operation-next-result operation))
         (name (service-name
                (register-result-service result))))
    ;; 2. register a service with the same name, but on a different
    ;; port for conflict
    (let ((conflict-service (copy-service (register-result-service result))))
      (incf (service-port conflict-service))
      (let* ((conflict-operation (register conflict-service :no-auto-rename t))
             (result (operation-next-result conflict-operation)))
        (is (register-result-p result))
        ;; 3. Zeroconf will automatically rename our first service
        ;; after some time.
        (let ((result (operation-next-result operation
                                             :timeout +test-long-timeout+)))
          (is (register-result-p result))
          (is-true (register-result-conflict-p result))
          ;; 4. The service has finally been renamed, compare that the
          ;; new name is different.
          (let ((result (operation-next-result operation
                                               :timeout +test-long-timeout+)))
            (is (register-result-p result))
            (is-false (register-result-conflict-p result))
            (is-false (string-equal name
                                    (service-name
                                     (register-result-service result))))))
        (is-true (cancel conflict-operation))))
    (is-true (cancel operation))))

(test (registration-identical-service-timeout :depends-on registration-conflict)
  (let* ((service *test-service*)
         (operation (register service))
         (result (operation-next-result operation)))
    (let* ((conflict-service (copy-service (register-result-service result)))
           (conflict-operation (register conflict-service :no-auto-rename t))
           (result (operation-next-result conflict-operation
                                          :timeout +test-timeout+)))
      (is (null result))
      (is-true (cancel conflict-operation)))
    (is-true (cancel operation))))

(test (enumerate-browse-domains :depends-on register-service)
  (let* ((operation (enumerate-domains :domains :browse-domains))
         (result (operation-next-result operation)))
    (is (enumerate-domains-result-p result))
    (is-true (cancel operation))))

(test (enumerate-registration-domains :depends-on register-service)
  (let* ((operation (enumerate-domains :domains :registration-domains))
         (result (operation-next-result operation)))
    (is (enumerate-domains-result-p result))
    (is-true (cancel operation))))

(test (browse :depends-on enumerate-browse-domains)
  (stop)
  (let* ((register-operation (register *test-service*))
         (register-result (operation-next-result register-operation)))
    (is (register-result-p register-result))
    (let* ((browse-operation
            (browse *test-service-type*
                    :domain (make-domain
                             :name (service-domain
                                    (register-result-service register-result)))))
           (browse-result (operation-next-result browse-operation)))
      (is (browse-result-p browse-result))
      (is (eq :add (browse-result-presence browse-result)))
      (let ((browsed-service (browse-result-service browse-result))
            (registered-service (register-result-service register-result)))
        (is (string-equal (service-name browsed-service)
                          (service-name registered-service)))
        (is (string-equal (service-type browsed-service)
                          (service-type registered-service)))
        (is (string-equal (service-domain browsed-service)
                          (service-domain registered-service))))
      (is-true (cancel register-operation))
      (mp:process-wait "Waiting for registered operation to be cancelled."
                       #'(lambda ()
                           (operation-cancelled-p register-operation)))
      (setf browse-result (operation-next-result browse-operation))
      (is (browse-result-p browse-result))
      (is (eq :remove (browse-result-presence browse-result)))
      (is-true (cancel browse-operation)))))

(test (resolve :depends-on browse)
  (let* ((register-operation (register *test-service*))
         (register-result (operation-next-result register-operation)))
    (is (register-result-p register-result))
    (let* ((browse-operation
            (browse *test-service-type*
                    :domain (make-domain
                             :name (service-domain
                                    (register-result-service register-result)))))
           (browse-result (operation-next-result browse-operation)))
      (is (browse-result-p browse-result))
      (is (eq :add (browse-result-presence browse-result)))
      (let* ((resolve-operation (resolve (browse-result-service browse-result)))
             (resolve-result (operation-next-result resolve-operation)))
        (is (resolve-result-p resolve-result))
        (is (service-equal (resolve-result-service resolve-result)
                           (register-result-service register-result)))
        (is-false (cancel resolve-operation)))
      (is-true (cancel browse-operation)))
    (is-true (cancel register-operation))))
