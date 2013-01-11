;;;; -*- mode: LISP; syntax: COMMON-LISP; indent-tabs-mode: nil -*-

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

;;; TXT-RECORD building and parsing.


(in-package #:com.wildora.dns-sd)

(defun bytes-to-string (sequence &key (start 0) (end (length sequence)))
  "Converts a sequence of bytes (unsigned-byte 8) to a string using ~
   the implementation's default character encoding."
  (ef:decode-external-string sequence :utf-8
                             :start start
                             :end end))

(defun string-to-bytes (string)
  "Converts a string to a sequence of bytes (unsigned-byte 8) using ~
   the implementation's default character encoding."
  (ef:encode-lisp-string string :utf-8))


;;;;
;;;; Following code (c) John Wiseman (jjwiseman@yahoo.com)
;;;; With modifications by Camille Troillard
;;;; https://github.com/wiseman/cl-zeroconf/blob/master/bonjour-apple-mdns.lisp
;;;; Licensed under the MIT license
;;;;

;; Section 6.1 of
;; http://files.dns-sd.org/draft-cheshire-dnsext-dns-sd.txt claims
;; that "when using Multicast DNS [mDNS] the maximum packet size is
;; 9000 bytes, which imposes an upper limit on the size of TXT records
;; of about 8800 bytes."  Maybe I should be checking for that.  See
;; also section 6.3 of the above document for more recommendations on
;; TXT record size.
;;
;; "As a general rule, attribute names that contain no dots are
;; defined as part of the open-standard definition written by the
;; person or group defining the DNS-SD profile for discovering that
;; particular service type. Vendor-specific extensions should be given
;; names of the form "keyname.company.com=value", using a domain name
;; legitimately registered to the person or organization creating the
;; vendor-specific key. This reduces the risk of accidental conflict
;; if different organizations each define their own vendor-specific
;; keys."

(defun build-txt-record (properties)
  ;; RFC 1035 doesn't allow a TXT record to contain *zero* strings,
  ;; so a single empty string is the smallest legal DNS TXT record.
  (unless properties
    (setq properties (list nil)))
  (let ((sequence (reduce #'(lambda (&rest args)
                              (apply 'concatenate 'vector args))
                          (mapcar #'(lambda (property)
                                      (let ((sub-record (build-property-sub-record property)))
                                        (concatenate 'vector
                                                     (vector (length sub-record))
                                                     sub-record)))
                                  properties))))
    (make-array (length sequence)
                :element-type '(unsigned-byte 8)
                :initial-contents sequence)))


(defconstant +key-value-separator+
  (char-code #\=))

(defun build-property-sub-record (property)
  (let ((result
         (when-let* ((key (car property))
                     (value (cdr property)))
           (check-type key string)
           (check-type value string)
           (string-to-bytes (format nil "~A=~A" key value)))))
    (assert (< (length result) 256))
    result))


;; Section 6.4 of
;; http://files.dns-sd.org/draft-cheshire-dnsext-dns-sd.txt says the
;; following:
;;
;;  * "Strings beginning with an '=' character (i.e. the name is
;;    missing) SHOULD be silently ignored"
;;
;;  * "Case is ignored when interpreting a name, so 'papersize=A4',
;;   'PAPERSIZE=A4' and 'Papersize=A4' are all identical."
;;
;;  * "Unless specified otherwise by a particular DNS-SD profile, a
;;    given attribute name may appear at most once in a TXT record. If
;;    a client receives a TXT record containing the same attribute
;;    name more than once, then the client SHOULD silently ignore all
;;    but the first occurrence of that attribute."

(defun parse-txt-record (record)
  "Parses a DNS-style TXT record (see
<http://www.zeroconf.org/Rendezvous/txtrecords.html>) into an
association list of (KEY . VALUE) pairs.  TXT record strings of the
form \"KEY\" result in a (KEY . NIL) pair, strings of the form
\"KEY=\" result in a (KEY . <empty vector>) pair, and strings of the
form \"KEY=VALUE\" result in a (KEY . VALUE) pair.  KEY is always a
string, and VALUE, if present, is a vector with elements of type
(unsigned-byte 8) (DNS TXT records can contain binary data)."
  (let ((properties '()))
    (map-txt-record #'(lambda (key value)
			(push (cons key value)
                              properties))
		    record)
    (reverse properties)))

(defun map-txt-record (fn record)
  (labels ((safe-min (a b)
	     (cond ((null a) b)
		   ((null b) a)
		   (t (min a b))))
	   (parse (pos)
	     (when (< pos (length record))
               (let ((len (elt record pos)))
                 (incf pos)
                 (when (> len 0)
                   (let* ((key-end-pos (position +key-value-separator+
                                                 record
                                                 :start pos))
                          (key (bytes-to-string record
                                                :start pos
                                                :end (safe-min key-end-pos
                                                               (+ pos len))))
                          (value (when key-end-pos
                                   (bytes-to-string record
                                                    :start (1+ key-end-pos)
                                                    :end (+ pos len)))))
                     (funcall fn key value)))
                 (parse (+ pos len))))))
    (parse 0)))
