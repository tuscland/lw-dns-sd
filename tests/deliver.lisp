;; -*- external-format: utf-8; -*-

(in-package "CL-USER")

(load-all-patches)

(load (current-pathname "../defsystem"))
(compile-system :dns-sd-tests :load t)

(defun run-tests-and-quit ()
  (format t "~&DNS-SD daemon version: ~A~%"
          (dns-sd:daemon-version))
  (dns-sd-tests:run-tests)
  (quit))

(compile 'run-tests-and-quit)

(defvar *target-executable-name* "dns-sd-tester")

(deliver 'run-tests-and-quit
         *target-executable-name*
         4
         :multiprocessing t)
