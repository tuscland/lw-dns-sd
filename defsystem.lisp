(in-package #:cl-user)

(defsystem dnssd-dependencies
  (:default-pathname "dependencies")
  :members (("infra" :type :system)
            "load"))

(defsystem dnssd
  (:default-pathname "src")
  :members (("dnssd-dependencies" :type :system)
            "if-name"
            "constants"
            "event"
            "conditions"
            "txt-record"
            "structs"
            "foreign"
            "operation"
            "dispatcher"
            "foreign-high"
            "api"
            "package")
  :rules ((:in-order-to :compile :all
           (:requires
            (:load "dnssd-dependencies")))
          (:in-order-to :compile "event"
           (:requires
            (:load "conditions")))
          (:in-order-to :compile "foreign"
           (:requires
            (:load "conditions")))
          (:in-order-to :compile "operation"
           (:requires
            (:load "conditions")
            (:load "foreign")
            (:load "event")))
          (:in-order-to :compile "dispatcher"
           (:requires
            (:load "operation")))
          (:in-order-to :compile "foreign-high"
           (:requires
            (:load "txt-record")
            (:load "structs")
            (:load "foreign")
            (:load "operation")))
          (:in-order-to :compile "api"
           (:requires
            (:load "constants")
            (:load "dispatcher")
            (:load "foreign-high")))
          (:in-order-to :compile "package"
           (:requires
            (:load "if-name")
            (:load "conditions")
            (:load "structs")
            (:load "event")
            (:load "operation")
            (:load "dispatcher")
            (:load "api")))))

(defsystem dnssd-tests
  (:default-pathname "tests")
  :members (("eos" :type :system)
            ("dnssd" :type :system)
            "package"
            "tests")
  :rules ((:in-order-to :compile :all
           (:requires  (:load :previous)))))
