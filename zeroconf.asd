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
               (:file "service-handle")
               (:file "record-handle")
               (:file "dispatcher")
               (:file "foreign")
               (:file "zeroconf")))
