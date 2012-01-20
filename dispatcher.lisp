(in-package #:zeroconf)


(defclass dispatcher ()
  ((handles
    :accessor dispatcher-handles
    :initform nil)
   (process
    :accessor dispatcher-process
    :initform nil)))


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
  (dolist (handle (copy-seq
                   (dispatcher-handles self)))
    (%dispatcher-remove-handle self handle)))


(defmethod dispatcher-loop ((self dispatcher))
  (mp:ensure-process-cleanup `(%dispatcher-cleanup ,self))
  (loop :with mailbox := (mp:process-mailbox (mp:get-current-process))
        :for handle := (sys:wait-for-input-streams-returning-first
                        (dispatcher-handles self)
                        :wait-reason "Waiting for Zeroconf events"
                        :wait-function #'(lambda ()
                                           #+lispworks6.1 (mp:mailbox-not-empty-p mailbox)
                                           #-lispworks6.1 (mp:mailbox-peek mailbox)))
        :do (with-simple-restart (abort "Return to event loop.")
              (if handle
                  (when (service-handle-process-result handle)
                    (%dispatcher-remove-handle self handle))
                (mp:process-all-events)))
        :while t))


(defmethod dispatcher-start ((self dispatcher))
  (when (dispatcher-running-p self)
    (error "Zeroconf Dispatcher is already started."))

  #+win32 (fli:register-module "dnssd")
  (assert (> (zeroconf:daemon-version) 0))

  (let* ((mp:*process-initial-bindings*
          (list* (cons '*standard-output* (or mp:*background-standard-output* *standard-output*))
                 mp:*process-initial-bindings*))
         (process (mp:process-run-function "Zeroconf Dispatcher"
                                           '(:mailbox t)
                                           #'dispatcher-loop self)))
    (setf (dispatcher-process self) process))
  self)


(defmethod dispatcher-send ((self dispatcher) object)
  (with-slots (process) self
    (mp:process-send process object)
    (mp:process-poke process)))


(defmethod dispatcher-stop ((self dispatcher))
  (if (dispatcher-running-p self)
      (progn
        (dispatcher-send self
                         #'(lambda ()
                             (mp:process-kill
                              (mp:get-current-process))))
        (mp:process-join (dispatcher-process self)
                         :timeout 10))
    (warn "Zeroconf Dispatcher not running"))  
  self)


(defmethod dispatcher-add-handle ((self dispatcher) handle)
  (check-type handle handle)
  (dispatcher-send self
                   `(%dispatcher-add-handle ,self ,handle))
  handle)


(defmethod dispatcher-remove-handle ((self dispatcher) handle)
  (check-type handle handle)
  (dispatcher-send self
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
