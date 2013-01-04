(in-package #:cl-user)

(defsystem #:dnssd
  (:default-pathname "src")
  :members ("dependencies"
            "package"
            "if-name"
            "constants"
            "conditions"
            "txt-record"
            "structs"
            "result"
            "foreign"
            "operation"
            "dispatcher"
            "core"
            "api")
  :rules ((:in-order-to :compile :all
           (:requires
            (:load "dependencies")
            (:load "package")
            (:load "constants")))
          (:in-order-to :compile ("result")
           (:requires
            (:load "conditions")))
          (:in-order-to :compile "operation"
           (:requires
            (:load "conditions")
            (:load "result")))))

(defsystem #:dnssd-tests
  (:default-pathname "tests")
  :members (("eos" :type :system)
            ("dnssd" :type :system)
            "package"
            "tests")
  :rules ((:in-order-to :compile :all
           (:requires  (:load :previous)))))
