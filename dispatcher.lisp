(in-package #:zeroconf)


(defclass dispatcher ()
  ((operations
    :accessor dispatcher-operations
    :initform nil)
   (process
    :accessor dispatcher-process
    :initform nil)))

(defmethod shared-initialize :after ((self dispatcher) slot-names &key)
  (dispatcher-start self))

(defmethod dispatcher-running-p ((self dispatcher))
  (mp:process-alive-p
   (dispatcher-process self)))

(defmethod %dispatcher-add-operation ((self dispatcher) operation)
  (push operation
        (dispatcher-operations self)))

(defmethod %dispatcher-remove-operation ((self dispatcher) operation)
  (setf (dispatcher-operations self)
        (remove operation
                (dispatcher-operations self)))
  (%cancel operation))

(defmethod %dispatcher-cleanup (process (self dispatcher))
  (dolist (operation (dispatcher-operations self))
    (%dispatcher-remove-operation self operation)))

(defmethod dispatcher-handle-message ((self dispatcher) message)
  (let ((command (car message))
        (operation (cdr message)))
    (ecase command
      (:add
       (%dispatcher-add-operation self operation))
      (:remove
       (%dispatcher-remove-operation self operation))
      (:stop       
       (mp:process-kill
        (mp:get-current-process))))))

(defmethod dispatcher-loop ((self dispatcher))
  (mp:ensure-process-cleanup `(%dispatcher-cleanup ,self))
  (loop with mailbox = (mp:process-mailbox
                        (mp:get-current-process))
        for operation = (sys:wait-for-input-streams-returning-first
                         (dispatcher-operations self)
                         :wait-reason "Waiting for Zeroconf events"
                         :wait-function #'(lambda ()
                                            (mp:mailbox-not-empty-p mailbox)))
        do (if operation
               (when (operation-process-result operation)
                 (%dispatcher-remove-operation self operation))
             (dispatcher-handle-message self
                                        (mp:mailbox-read mailbox)))
        while t))

(defmethod dispatcher-send-message ((self dispatcher) message)
  (unless (dispatcher-running-p self)
    (error "Zeroconf Dispatcher is stopped."))
  (mp:mailbox-send (mp:process-mailbox
                    (dispatcher-process self))
                   message))

(defmethod dispatcher-start ((self dispatcher))
  (when (dispatcher-running-p self)
    (error "Zeroconf Dispatcher is already started."))
  (let ((process (mp:process-run-function "DNS-SD Event Dispatcher"
                                          '(:mailbox t)
                                          'dispatcher-loop
                                          self)))
    (setf (dispatcher-process self) process)))

(defmethod dispatcher-stop ((self dispatcher))
  (unless (dispatcher-running-p self)
    (error "Zeroconf Dispatcher is already stopped."))
  (dispatcher-send-message self (cons :stop nil))
  (mp:process-wait "Waiting for Zeroconf Dispatcher to stop"
                   #'(lambda ()
                       (not
                        (dispatcher-running-p self)))))

(defmethod dispatcher-add-operation ((self dispatcher) operation)
  (assert operation)
  (dispatcher-send-message self
                                 (cons :add operation))
  operation)

(defmethod dispatcher-remove-operation ((self dispatcher) operation)
  (assert operation)
  (dispatcher-send-message self
                                 (cons :remove operation)))

(defmethod print-object ((self dispatcher) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream
            "(~:[STOPPED~;RUNNING~], ~[no~:;~:*~D~] pending operation~:P)"
            (dispatcher-running-p self)
            (length
             (dispatcher-operations self)))))



(defvar *dispatcher* (make-instance 'dispatcher))

(defmethod dispatch-operation ((self operation))
  (dispatcher-add-operation *dispatcher* self))

(defmethod cancel-operation ((self operation))
  (dispatcher-remove-operation *dispatcher* self))
