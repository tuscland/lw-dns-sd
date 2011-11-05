;;;; zeroconf.asd

(asdf:defsystem #:zeroconf
  :serial t
  :components ((:file "package")
               (:file "debugging")
               (:file "network-utilities")
               (:file "conditions")
               (:file "txt-record")

               (:file "common")
               (:file "service-handle")
               (:file "record-handle")
               (:file "dispatcher")
               (:file "constants")
               (:file "foreign")
               (:file "zeroconf")))
