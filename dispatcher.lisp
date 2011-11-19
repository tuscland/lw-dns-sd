(in-package #:zeroconf)


(defclass dispatcher ()
  ((handles
    :accessor dispatcher-handles
    :initform nil)
   (process
    :accessor dispatcher-process
    :initform nil)))

(defmethod shared-initialize :after ((self dispatcher) slot-names &key)
  (dispatcher-start self))

(defmethod dispatcher-running-p ((self dispatcher))
  (mp:process-alive-p
   (dispatcher-process self)))

(defmethod %dispatcher-add-handle ((self dispatcher) handle)
  (push handle
        (dispatcher-handles self)))

(defmethod %dispatcher-remove-handle ((self dispatcher) handle)
  (setf (dispatcher-handles self)
        (remove handle
                (dispatcher-handles self)))
  (%cancel handle))

(defmethod %dispatcher-cleanup (process (self dispatcher))
  (dolist (handle (dispatcher-handles self))
    (%dispatcher-remove-handle self handle)))

(defmethod dispatcher-loop ((self dispatcher))
  (mp:ensure-process-cleanup `(%dispatcher-cleanup ,self))
  (loop with mailbox = (mp:process-mailbox
                        (mp:get-current-process))
        for handle = (sys:wait-for-input-streams-returning-first
                      (dispatcher-handles self)
                      :wait-reason "Waiting for Zeroconf events"
                      :wait-function #'(lambda ()
                                         (mp:mailbox-not-empty-p mailbox)))
        do 
        (if handle
               (when (service-handle-process-result handle)
                 (%dispatcher-remove-handle self handle))
             (mp:process-all-events))
        while t))

(defmethod dispatcher-start ((self dispatcher))
  (when (dispatcher-running-p self)
    (error "Zeroconf Dispatcher is already started."))
  (let ((process (mp:process-run-function "DNS-SD Event Dispatcher"
                                          '(:mailbox t)
                                          'dispatcher-loop
                                          self)))
    (setf (dispatcher-process self) process)))

(defmethod dispatcher-stop ((self dispatcher))
  (mp:process-send
   (dispatcher-process self)
   #'(lambda ()
       (mp:process-kill
        (mp:get-current-process))))
  (mp:process-wait "Waiting for Zeroconf Dispatcher to stop"
                   #'(lambda ()
                       (not
                        (dispatcher-running-p self)))))

(defmethod dispatcher-add-handle ((self dispatcher) handle)
  (assert handle)
  (mp:process-send
   (dispatcher-process self)
   `(%dispatcher-add-handle ,self ,handle))
  handle)

(defmethod dispatcher-remove-handle ((self dispatcher) handle)
  (assert handle)
  (mp:process-send
   (dispatcher-process self)
   `(%dispatcher-remove-handle ,self ,handle)))

(defmethod print-object ((self dispatcher) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream
            "(~:[STOPPED~;RUNNING~], ~[no~:;~:*~D~] pending handle~:P)"
            (dispatcher-running-p self)
            (length
             (dispatcher-handles self)))))



(defvar *dispatcher* (make-instance 'dispatcher))

(defmethod dispatch ((self service-handle))
  (dispatcher-add-handle *dispatcher* self))

(defmethod cancel ((self service-handle))
  (dispatcher-remove-handle *dispatcher* self))

(defun start ()
  (dispatcher-start *dispatcher*))

(defun stop ()
  (dispatcher-stop *dispatcher*))
