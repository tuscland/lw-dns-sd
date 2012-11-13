(in-package #:zeroconf)


(defun bytes-to-string (sequence &key (start 0) (end (length sequence)) (external-format :utf-8))
  "Converts a sequence of bytes (unsigned-byte 8) to a string using ~
   the implementation's default character encoding."
  (ef:decode-external-string sequence external-format :start start :end end))

(defun string-to-bytes (string)
  "Converts a string to a sequence of bytes (unsigned-byte 8) using ~
   the implementation's default character encoding."
  (ef:encode-lisp-string string :utf-8))


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

(fli:define-c-struct (if-nameindex
                      (:foreign-name "if_nameindex"))
  (index (:unsigned :int))
  (name (:pointer :char)))

(fli:define-foreign-function (%if-nameindex "if_nameindex" :source)
    ()
  :result-type (:ptr (:struct if-nameindex))
  :language :ansi-c)

(fli:define-foreign-function (%if-freenameindex "if_freenameindex" :source)
    ((ptr (:pointer (:struct if-nameindex))))
  :result-type :void
  :language :ansi-c)

(defun if-name-index ()
  (let ((result (%if-nameindex)))
    (unless (fli:null-pointer-p result)
      (loop :with nameindex := (fli:copy-pointer result)
            :for index := (fli:foreign-slot-value nameindex 'index)
            :while (not (zerop index))
            :collect (cons index
                           (fli:convert-from-foreign-string
                            (fli:foreign-slot-value nameindex 'name)))
            :do (fli:incf-pointer nameindex)
            :finally (%if-freenameindex result)))))