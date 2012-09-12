(in-package #:cl-user)

(defsystem zeroconf
  (:default-pathname "src")
  :members (("infra" :type :system)
            "package"
            "debugging"
            "utils"
            "conditions"
            "txt-record"
            "constants"
            "structs"
            "operation"
            "service-operation"
;            "record-handle"
            "dispatcher"
            "foreign"
            "zeroconf")
  :rules ((:in-order-to :compile "package"
           (:requires (:load "infra")))
          (:in-order-to :compile "structs"
           (:requires (:load "constants")))
          (:in-order-to :compile "service-operation"
           (:requires (:load "structs")))
          (:in-order-to :compile "foreign"
           (:requires (:load "structs")
                      (:load "service-operation")))))

(defsystem zeroconf-tests
  (:default-pathname "tests")
  :members (("eos" :type :system)
            ("zeroconf" :type :system)
            "package"
            "suite"
            "tests")
  :rules ((:in-order-to :compile :all
           (:caused-by (:compile :previous))
           (:requires  (:load :previous)))))
