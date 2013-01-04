(in-package #:com.wildora.dnssd)

(defclass result ()
  ())

(defgeneric result-property (result property-name)
  (:method ((result result) (property-name symbol))
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
  ;; TODO: test property membership properly
  (unless (member property (result-properties self))
    (error "Result ~A does have property ~A" self property))
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
  (make-instance 'error-result :error error))

(defun make-result (more-coming-p properties)
  (make-instance 'reply-result
                 :more-coming-p more-coming-p
                 :properties properties))
