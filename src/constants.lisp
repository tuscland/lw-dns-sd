(in-package #:zeroconf)

(defconstant +service-class-IN+ 1) ; Internet

(defconstant +service-type-A+        1)     ; Host address.
(defconstant +service-type-NS+       2)     ; Authoritative server.
(defconstant +service-type-MD+       3)     ; Mail destination.
(defconstant +service-type-MF+       4)     ; Mail forwarder.
(defconstant +service-type-CNAME+    5)     ; Canonical name.
(defconstant +service-type-SOA+      6)     ; Start of authority zone.
(defconstant +service-type-MB+       7)     ; Mailbox domain name.
(defconstant +service-type-MG+       8)     ; Mail group member.
(defconstant +service-type-MR+       9)     ; Mail rename name.
(defconstant +service-type-NULL+     10)    ; Null resource record.
(defconstant +service-type-WKS+      11)    ; Well known service.
(defconstant +service-type-PTR+      12)    ; Domain name pointer.
(defconstant +service-type-HINFO+    13)    ; Host information.
(defconstant +service-type-MINFO+    14)    ; Mailbox information.
(defconstant +service-type-MX+       15)    ; Mail routing information.
(defconstant +service-type-TXT+      16)    ; One or more text strings (NOT "zero or more...").
(defconstant +service-type-RP+       17)    ; Responsible person.
(defconstant +service-type-AFSDB+    18)    ; AFS cell database.
(defconstant +service-type-X25+      19)    ; X_25 calling address.
(defconstant +service-type-ISDN+     20)    ; ISDN calling address.
(defconstant +service-type-RT+       21)    ; Router.
(defconstant +service-type-NSAP+     22)    ; NSAP address.
(defconstant +service-type-NSAP_PTR+ 23)    ; Reverse NSAP lookup (deprecated).
(defconstant +service-type-SIG+      24)    ; Security signature.
(defconstant +service-type-KEY+      25)    ; Security key.
(defconstant +service-type-PX+       26)    ; X.400 mail mapping.
(defconstant +service-type-GPOS+     27)    ; Geographical position (withdrawn).
(defconstant +service-type-AAAA+     28)    ; IPv6 Address.
(defconstant +service-type-LOC+      29)    ; Location Information.
(defconstant +service-type-NXT+      30)    ; Next domain (security).
(defconstant +service-type-EID+      31)    ; Endpoint identifier.
(defconstant +service-type-NIMLOC+   32)    ; Nimrod Locator.
(defconstant +service-type-SRV+      33)    ; Server Selection.
(defconstant +service-type-ATMA+     34)    ; ATM Address
(defconstant +service-type-NAPTR+    35)    ; Naming Authority PoinTeR
(defconstant +service-type-KX+       36)    ; Key Exchange
(defconstant +service-type-CERT+     37)    ; Certification record
(defconstant +service-type-A6+       38)    ; IPv6 Address (deprecated)
(defconstant +service-type-DNAME+    39)    ; Non-terminal DNAME (for IPv6)
(defconstant +service-type-SINK+     40)    ; Kitchen sink (experimental)
(defconstant +service-type-OPT+      41)    ; EDNS0 option (meta-RR)
(defconstant +service-type-APL+      42)    ; Address Prefix List
(defconstant +service-type-DS+       43)    ; Delegation Signer
(defconstant +service-type-SSHFP+    44)    ; SSH Key Fingerprint
(defconstant +service-type-IPSECKEY+ 45)    ; IPSECKEY
(defconstant +service-type-RRSIG+    46)    ; RRSIG
(defconstant +service-type-NSEC+     47)    ; Denial of Existence
(defconstant +service-type-DNSKEY+   48)    ; DNSKEY
(defconstant +service-type-DHCID+    49)    ; DHCP Client Identifier
(defconstant +service-type-NSEC3+      50)  ; Hashed Authenticated Denial of Existence
(defconstant +service-type-NSEC3PARAM+ 51)  ; Hashed Authenticated Denial of Existence

(defconstant +service-type-HIP+      55)    ; Host Identity Protocol

(defconstant +service-type-SPF+      99)    ; Sender Policy Framework for E-Mail
(defconstant +service-type-UINFO+    100)   ; IANA-Reserved
(defconstant +service-type-UID+      101)   ; IANA-Reserved
(defconstant +service-type-GID+      102)   ; IANA-Reserved
(defconstant +service-type-UNSPEC+   103)   ; IANA-Reserved

(defconstant +service-type-TKEY+     249)   ; Transaction key
(defconstant +service-type-TSIG+     250)   ; Transaction signature.
(defconstant +service-type-IXFR+     251)   ; Incremental zone transfer.
(defconstant +service-type-AXFR+     252)   ; Transfer zone of authority.
(defconstant +service-type-MAILB+    253)   ; Transfer mailbox records.
(defconstant +service-type-MAILA+    254)   ; Transfer mail agent records.
(defconstant +service-type-ANY+      255)   ; Wildcard match.

(defconstant +interface-index-any+        #x00000000)
(defconstant +interface-index-local-only+ #xFFFFFFFF)
(defconstant +interface-index-unicast+    #xFFFFFFFE)
(defconstant +interface-index-p2p+        #xFFFFFFFD)

(defconstant +protocol-ipv4+             #x001)
(defconstant +protocol-ipv6+             #x002)

(defconstant +no-err+                    #x000)
(defconstant +flag-more-coming+          #x001)
(defconstant +flag-add+                  #x002)
(defconstant +flag-default+              #x004)
(defconstant +flag-no-auto-rename+       #x008)
(defconstant +flag-shared+               #x010)
(defconstant +flag-unique+               #x020)
(defconstant +flag-browse-domains+       #x040)
(defconstant +flag-registration-domains+ #x080)
(defconstant +flag-long-lived-query+     #x100)
(defconstant +flag-force-multicast+      #x400)
(defconstant +flag-no-flag+              #x000)

(defconstant +max-service-name-length+   63)
;; FIXME: checks are buggy because the length accounts for the *escaped* string
(defconstant +max-domain-name-length+  1008)

(defun flag-test (flag flags &optional (included-symbol t) excluded-symbol)
  (if (zerop (logand flag flags))
      excluded-symbol
    included-symbol))


(defparameter *property-daemon-version* "DaemonVersion")

(defparameter *meta-query-service-full-name* "_services._dns-sd._udp.local.")
