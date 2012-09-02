(defsystem zeroconf ()
  :members (("infra" :type :system)
            "package"
            "debugging"
            "network-utilities"
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
  :rules ((:in-order-to :compile :all
           (:caused-by (:compile :previous))
           (:requires  (:load :previous)))))

(defsystem zeroconf-tests (:default-pathname "tests")
  :members (("eos" :type :system)
            ("zeroconf" :type :system)
            "package"
            "suite"
            "tests")
  :rules ((:in-order-to :compile :all
           (:caused-by (:compile :previous))
           (:requires  (:load :previous)))))
