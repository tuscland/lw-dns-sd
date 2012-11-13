(in-package #:zeroconf)

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
  (flet ((concat (&rest args)
	   (apply #'concatenate 'vector args)))
    ;; RFC 1035 doesn't allow a TXT record to contain *zero* strings,
    ;; so a single empty string is the smallest legal DNS TXT record.
    (unless properties
      (setq properties (list nil)))
    (let ((sequence (reduce #'concat
                            (mapcar #'(lambda (property)
                                        (let ((sub-record (build-property-sub-record property)))
                                          (assert (<= (length sub-record) 255))
                                          (concatenate 'vector
                                                       (vector (length sub-record))
                                                       sub-record)))
                                    properties))))
      ;; array must be static so it can be made available to FLI as a pointer
      (make-array (length sequence)
                  :element-type '(unsigned-byte 8)
                  :initial-contents sequence
                  :allocation :static))))



(defconstant +key-value-separator+ 61) ;; #\= in ASCII.

(defun build-property-sub-record (property)
  (let ((key (car property))
	(value (cdr property)))
    (if (null value)
	(string-to-bytes key)
      (if (null value)
          (string-to-bytes (format nil "~A=" key))
        (if (stringp value)
            (string-to-bytes (format nil "~A=~A" key value))
          (concatenate 'vector
                       (string-to-bytes key)
                       (list +key-value-separator+)
                       value))))))


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
  "Parses a Zeroconf-style TXT record (see
<http://www.zeroconf.org/Rendezvous/txtrecords.html>) into an
association list of \(KEY . VALUE) pairs.  TXT record strings of the
form \"KEY\" result in a \(KEY . NIL) pair, strings of the form
\"KEY=\" result in a \(KEY . <empty vector>) pair, and strings of the
form \"KEY=VALUE\" result in a \(KEY . VALUE) pair.  KEY is always a
string, and VALUE, if present, is a vector with elements of type
\(unsigned-byte 8) \(Zeroconf TXT records can contain binary data)."
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
		   (T (min a b))))
	   (parse (pos)
	     (if (>= pos (length record))
		 (values)
		 (let* ((len (elt record pos)))
		   (when (> len 0)
		     (let ((key-end-pos (position +key-value-separator+ record :start (+ pos 1))))
		       (let ((key (bytes-to-string record
                                                   :start (+ pos 1)
                                                   :end (safe-min key-end-pos (+ pos 1 len))))
			     (value (if key-end-pos
                                        (bytes-to-string record
                                                         :start (+ key-end-pos 1)
                                                         :end(+ pos len 1))
					nil)))
			 (funcall fn key value))))
		   (parse (+ pos len 1))))))
    (parse 0)))
