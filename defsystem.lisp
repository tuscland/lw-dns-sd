(in-package #:cl-user)

(defsystem dnssd
  (:default-pathname "src")
  :members (("infra" :type :system)
            "if-name"
            "constants"
            "result"
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
            (:load "infra")))
          (:in-order-to :compile "foreign"
           (:requires
            (:load "conditions")))
          (:in-order-to :compile "operation"
           (:requires
            (:load "conditions")
            (:load "foreign")
            (:load "result")))
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
            (:load "result")
            (:load "operation")
            (:load "dispatcher")
            (:load "api")))))

(defsystem dnssd-tests
  (:default-pathname "tests")
  :members (("eos" :type :system)
            ("dnssd" :type :system)
            "package"
            "suite"
            "tests")
  :rules ((:in-order-to :compile :all
           (:requires  (:load :previous)))))
