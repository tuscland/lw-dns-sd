(in-package #:zeroconf)

;; From cl-swap
(declaim (inline swap-bytes-16))
(defun swap-bytes-16 (value)
  (declare (type (unsigned-byte 16) value)
           (optimize (speed 3) (safety 0) (debug 0) (hcl:fixnum-safety 0)))
  (logior (ash (logand #xFF value)  8)
          (ash value -8)))

(defun ntohs (value)
  #+:little-endian
  (swap-bytes-16 value)
  #+:big-endian
  value)

(defun htons (value)
  #+:little-endian
  (swap-bytes-16 value)
  #+:big-endian
  value)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant IFNAMSIZ 16)
  #+win32
  (fli:register-module "Iphlpapi"))

(fli:define-foreign-function (if-name-to-index "if_nametoindex" :source)
    ((name (:reference-pass (:ef-mb-string :limit IFNAMSIZ))))
  :result-type (:unsigned :int)
  :language :ansi-c)

(fli:define-foreign-function (%if-index-to-name "if_indextoname" :source)
    ((index (:unsigned :int))
     (name  :pointer))
  :result-type :pointer
  :language :ansi-c)

(defun if-index-to-name (index)
  (fli:with-dynamic-foreign-objects ((name (:ef-mb-string :limit IFNAMSIZ)))
    (let ((result (%if-index-to-name index name)))
      (unless (fli:pointer-eq result fli:*null-pointer*)
        (fli:convert-from-foreign-string name)))))
