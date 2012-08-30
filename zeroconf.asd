;;;; zeroconf.asd

(asdf:defsystem #:zeroconf
  :serial t
  :depends-on (#:infra)
  :components ((:file "package")
               (:file "debugging")
               (:file "network-utilities")
               (:file "conditions")
               (:file "txt-record")

               (:file "constants")
               (:file "common")
               (:file "operation")
               (:file "service-operation")
               (:file "record-handle")
               (:file "dispatcher")
               (:file "foreign")
               (:file "zeroconf")))

(asdf:defsystem #:zeroconf-tests
  :depends-on (:eos :zeroconf)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "tests" :depends-on ("suite"))))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :zeroconf))))
  (asdf:oos 'asdf:load-op :zeroconf-tests)
  (asdf:oos 'asdf:test-op :zeroconf-tests))
