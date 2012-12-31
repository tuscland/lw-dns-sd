(defpackage com.wildora.dnssd.event
  (:import-from #:com.wildora.dnssd.conditions
   #:dnssd-error))

(in-package #:com.wildora.dnssd.event)


(define-condition event-property-error (dnssd-error)
  ())


(defclass event ()
  ())

(defgeneric event-property (event property-name)
  (:method ((event event) (property-name symbol))
   (error "Event ~A does not have properties" event)))

(defgeneric event-more-coming-p (event)
  (:method ((event event))
   nil))

(defgeneric check-event (event)
  (:method ((event event))
   event))

(defclass reply-event (event)
  ((properties
    :reader event-properties
    :initform nil
    :initarg :properties)
   (more-coming-p
    :reader event-more-coming-p
    :initform nil
    :initarg :more-coming-p)))

(defmethod event-property ((self reply-event) (property symbol))
  (unless (member property (event-properties self))
    (error 'event-property-error
           :format-control "Event ~A does have property ~A"
           :format-arguments (list self property)))
  (getf (event-properties self)
        property))

(defmethod print-object ((self reply-event) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "MORE-COMING: ~A, ~S"
            (event-more-coming-p self)
            (event-properties self))))


(defclass error-event (event)
  ((error
    :reader error-event-error
    :initform nil
    :initarg :error)))

(defmethod check-event ((self error-event))
  (error (error-event-error self)))

(defun make-operation-error-event (error)
  (make-instance 'error-event
                 :error error))

(defun make-event (more-coming-p properties)
  (make-instance 'reply-event
                 :more-coming-p more-coming-p
                 :properties properties))
