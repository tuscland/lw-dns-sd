(in-package #:zeroconf)

(defclass dispatcher ()
  ((operations
    :accessor dispatcher-operations
    :initform nil)
   (process
    :accessor dispatcher-process
    :initform nil)))

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
  (dolist (operation (copy-seq
                      (dispatcher-operations self)))
    (%dispatcher-remove-operation self operation)))

(defmethod dispatcher-wait-reason ((self dispatcher))
  (format nil "Waiting, ~[no~:;~:*~D~] pending operation~:P"
          (length
           (dispatcher-operations self))))

(defmethod dispatcher-loop ((self dispatcher))
  (mp:ensure-process-cleanup `(%dispatcher-cleanup ,self))
  (loop :with mailbox := (mp:process-mailbox (mp:get-current-process))
        :for operation := (sys:wait-for-input-streams-returning-first
                           (dispatcher-operations self)
                           :wait-reason (dispatcher-wait-reason self)
                           :wait-function #'(lambda ()
                                              #+lispworks6.1 (mp:mailbox-not-empty-p mailbox)
                                              #-lispworks6.1 (mp:mailbox-peek mailbox)))
        :do (with-simple-restart (abort "Return to event loop.")
              (if operation
                  (when (service-operation-process-result operation)
                    (%dispatcher-remove-operation self operation))
                (mp:process-all-events)))
        :while t))

(defmethod dispatcher-start ((self dispatcher))
  (when (dispatcher-running-p self)
    (error 'zeroconf-error :format-control "Zeroconf Dispatcher is already started."))

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

(defmethod dispatcher-send ((self dispatcher) form)
  (with-slots (process) self
    (mp:process-send process form)
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

(defmethod dispatcher-add-operation ((self dispatcher) operation)
  (check-type operation operation)
  (dispatcher-send self
                   `(%dispatcher-add-operation ,self ,operation))
  operation)

(defmethod dispatcher-remove-operation ((self dispatcher) operation)
  (check-type operation operation)
  (dispatcher-send self
                   `(%dispatcher-remove-operation ,self ,operation)))

(defmethod print-object ((self dispatcher) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream
            "(~:[STOPPED~;RUNNING~], ~[no~:;~:*~D~] pending operation~:P)"
            (dispatcher-running-p self)
            (length
             (dispatcher-operations self)))))

(hcl:defglobal-variable *dispatcher* (make-instance 'dispatcher))

(defmethod dispatch ((self service-operation))
  ;; TODO: warn?
  (when (not (dispatcher-running-p *dispatcher*))
    (start))
  (dispatcher-add-operation *dispatcher* self))

(defmethod cancel ((self service-operation))
  ;; return t if the handle is not yet cancelled
  (prog1
      (not (null
            (operation-handle self)))
    (dispatcher-remove-operation *dispatcher* self)))

(defun start ()
  ;; TODO: support start completion notification
  (dispatcher-start *dispatcher*))

(defun stop ()
  ;; NOTE: synchronous call
  (dispatcher-stop *dispatcher*))

(defun running-p ()
  (dispatcher-running-p *dispatcher*))
