(in-package #:cl-user)

(defsystem #:dnssd
  (:default-pathname "src")
  :members (("infra" :type :system)
            "dependencies"
            "package"
            "if-name"
            "constants"
            "conditions"
            "txt-record"
            "structs"
            "event"
            "foreign"
            "operation"
            "dispatcher"
            "core"
            "api")
  :rules ((:in-order-to :compile :all
           (:requires
            (:load "infra")
            (:load "dependencies")
            (:load "package")))
          (:in-order-to :compile ("event")
           (:requires
            (:load "conditions")))
          (:in-order-to :compile "operation"
           (:requires
            (:load "conditions")
            (:load "event")))
          (:in-order-to :compile "core"
           (:requires
            (:load "structs")))
          (:in-order-to :compile "api"
           (:requires
            (:load "constants")))))

(defsystem #:dnssd-tests
  (:default-pathname "tests")
  :members (("eos" :type :system)
            ("dnssd" :type :system)
            "package"
            "tests")
  :rules ((:in-order-to :compile :all
           (:requires  (:load :previous)))))
