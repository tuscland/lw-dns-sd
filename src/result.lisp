(defpackage com.wildora.dnssd.result)

(in-package #:com.wildora.dnssd.result)


(defclass result ()
  ())

(defgeneric result-property (result property)
  (:method ((result result) (property symbol))
   (error "Result ~A does not have properties" result)))

(defgeneric result-more-coming-p (result)
  (:method ((result result))
   nil))

(defgeneric check-result (result)
  (:method ((result result))
   result))

(defclass reply-result (result)
  ((properties
    :reader result-properties
    :initform nil
    :initarg :properties)
   (more-coming-p
    :reader result-more-coming-p
    :initform nil
    :initarg :more-coming-p)))

(defmethod result-property ((self reply-result) (property symbol))
  (unless (member property (result-properties self))
    (error "Unknown property: ~A" property))
  (getf (result-properties self)
        property))

(defmethod print-object ((self reply-result) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "MORE-COMING: ~A, ~S"
            (result-more-coming-p self)
            (result-properties self))))


(defclass error-result (result)
  ((error
    :reader error-result-error
    :initform nil
    :initarg :error)))

(defmethod check-result ((self error-result))
  (error (error-result-error self)))

(defun make-operation-error-result (error)
  (make-instance 'error-result
                 :error error))

(defun make-result (more-coming-p properties)
  (make-instance 'reply-result
                 :more-coming-p more-coming-p
                 :properties properties))
