;;;; -*- encoding: utf-8; mode: LISP; syntax: COMMON-LISP; indent-tabs-mode: nil -*-

;;; DNS Service Discovery for LispWorks.
;;; Copyright (c) 2013, Camille Troillard. All rights reserved.

;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing,
;;; software distributed under the License is distributed on an "AS
;;; IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
;;; express or implied.  See the License for the specific language
;;; governing permissions and limitations under the License.

;;; Constants and keywords used throughout the system.


(in-package "COM.WILDORA.DNS-SD")

(defvar *library-version* "1.0.0")
(defparameter *default-timeout* 60)

(defconstant +interface-index-any+        #x00000000)
(defconstant +interface-index-local-only+ #xFFFFFFFF)
(defconstant +interface-index-unicast+    #xFFFFFFFE)
(defconstant +interface-index-p2p+        #xFFFFFFFD)

(defconstant +protocol-ipv4+              #x001)
(defconstant +protocol-ipv6+              #x002)
(defconstant +protocol-udp+               #x010)
(defconstant +protocol-tcp+               #x020)

(defconstant +flag-no-flag+               #x000)
(defconstant +flag-more-coming+           #x001)
(defconstant +flag-add+                   #x002)
(defconstant +flag-default+               #x004)
(defconstant +flag-no-auto-rename+        #x008)
(defconstant +flag-shared+                #x010)
(defconstant +flag-unique+                #x020)
(defconstant +flag-browse-domains+        #x040)
(defconstant +flag-registration-domains+  #x080)
(defconstant +flag-long-lived-query+      #x100)
(defconstant +flag-force-multicast+       #x400)
(defconstant +flag-force+                 #x800)


;;;;
;;;; Resource record classes
;;;;

;;; There is a single rrclass to handle right now.

(defun rrclass-to-keyword (rrclass)
  (assert (= rrclass 1))
  :IN)

(defun keyword-to-rrclass (keyword)
  (assert (eq keyword :IN))
  1)


;;;;
;;;; Resource record types
;;;;

(defvar *rrtypes-alist*
  '((:A .         1)    ; Host address.
    (:NS .        2)    ; Authoritative server.
    (:MD .        3)    ; Mail destination.
    (:MF .        4)    ; Mail forwarder.
    (:CNAME .     5)    ; Canonical name.
    (:SOA .       6)    ; Start of authority zone.
    (:MB .        7)    ; Mailbox domain name.
    (:MG .        8)    ; Mail group member.
    (:MR .        9)    ; Mail rename name.
    (:NULL .     10)    ; Null resource record.
    (:WKS .      11)    ; Well known service.
    (:PTR .      12)    ; Domain name pointer.
    (:HINFO .    13)    ; Host information.
    (:MINFO .    14)    ; Mailbox information.
    (:MX .       15)    ; Mail routing information.
    (:TXT .      16)    ; One or more text strings (NOT "zero or more...").
    (:RP .       17)    ; Responsible person.
    (:AFSDB .    18)    ; AFS cell database.
    (:X25 .      19)    ; X_25 calling address.
    (:ISDN .     20)    ; ISDN calling address.
    (:RT .       21)    ; Router.
    (:NSAP .     22)    ; NSAP address.
    (:NSAP_PTR . 23)    ; Reverse NSAP lookup (deprecated).
    (:SIG .      24)    ; Security signature.
    (:KEY .      25)    ; Security key.
    (:PX .       26)    ; X.400 mail mapping.
    (:GPOS .     27)    ; Geographical position (withdrawn).
    (:AAAA .     28)    ; IPv6 Address.
    (:LOC .      29)    ; Location Information.
    (:NXT .      30)    ; Next domain (security).
    (:EID .      31)    ; Endpoint identifier.
    (:NIMLOC .   32)    ; Nimrod Locator.
    (:SRV .      33)    ; Server Selection.
    (:ATMA .     34)    ; ATM Address
    (:NAPTR .    35)    ; Naming Authority PoinTeR
    (:KX .       36)    ; Key Exchange
    (:CERT .     37)    ; Certification record
    (:A6 .       38)    ; IPv6 Address (deprecated)
    (:DNAME .    39)    ; Non-terminal DNAME (for IPv6)
    (:SINK .     40)    ; Kitchen sink (experimental)
    (:OPT .      41)    ; EDNS0 option (meta-RR)
    (:APL .      42)    ; Address Prefix List
    (:DS .       43)    ; Delegation Signer
    (:SSHFP .    44)    ; SSH Key Fingerprint
    (:IPSECKEY . 45)    ; IPSECKEY
    (:RRSIG .    46)    ; RRSIG
    (:NSEC .     47)    ; Denial of Existence
    (:DNSKEY .   48)    ; DNSKEY
    (:DHCID .    49)    ; DHCP Client Identifier
    (:NSEC3 .    50)    ; Hashed Authenticated Denial of Existence
    (:NSEC3PARAM . 51)  ; Hashed Authenticated Denial of Existence
    (:HIP .      55)    ; Host Identity Protocol

    (:SPF .      99)    ; Sender Policy Framework for E-Mail
    (:UINFO .   100)    ; IANA-Reserved
    (:UID .     101)    ; IANA-Reserved
    (:GID .     102)    ; IANA-Reserved
    (:UNSPEC .  103)    ; IANA-Reserved
    (:TKEY .    249)    ; Transaction key
    (:TSIG .    250)    ; Transaction signature.
    (:IXFR .    251)    ; Incremental zone transfer.
    (:AXFR .    252)    ; Transfer zone of authority.
    (:MAILB .   253)    ; Transfer mailbox records.
    (:MAILA .   254)    ; Transfer mail agent records.
    (:ANY .     255)))  ; Wildcard match.

(defun rrtype-to-keyword (rrtype)
  (or (car
       (find rrtype
             *rrtypes-alist*
             :test '=
             :key 'cdr))
      (error "Invalid RRTYPE value ~S" rrtype)))

(defun keyword-to-rrtype (keyword)
  (or (cdr
       (find keyword
             *rrtypes-alist*
             :test 'eq
             :key 'car))
      (error "Invalid RRTYPE keyword ~S" keyword)))
