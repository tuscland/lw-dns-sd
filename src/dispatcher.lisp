(defpackage com.wildora.dnssd.dispatcher
  (:import-from #:com.wildora.dnssd.operation
   #:operation
   #:operation-cancelled-p
   #:cancel-operation
   #:process-operation
   #:daemon-version))

(in-package #:com.wildora.dnssd.dispatcher)

(hcl:defglobal-variable *operations* nil)
(hcl:defglobal-variable *process* nil)
(defconstant +process-join-timeout+ 10)


(defmethod %add-operation (operation)
  (push operation *operations*))

(defmethod %remove-operation (operation)
  (setf *operations*
        (remove operation *operations*))
  (cancel-operation operation))

(defmethod %cleanup (process)
  (dolist (operation (copy-seq *operations*))
    (%remove-operation operation)))

(defmethod dispatcher-wait-reason ()
  (format nil "Waiting, ~[no~:;~:*~D~] pending operation~:P"
          (length *operations*)))

(defmethod dispatcher-loop ()
  (mp:ensure-process-cleanup `%cleanup)
  (loop :with mailbox := (mp:process-mailbox (mp:get-current-process))
        :for operation := (sys:wait-for-input-streams-returning-first
                           *operations*
                           :wait-reason (dispatcher-wait-reason)
                           :wait-function #'(lambda ()
                                              #+lispworks6.1 (mp:mailbox-not-empty-p mailbox)
                                              #-lispworks6.1 (mp:mailbox-peek mailbox)))
        :do (with-simple-restart (abort "Return to event loop.")
              (if operation
                  (when (process-operation operation)
                    (%remove-operation operation))
                (mp:process-all-events)))
        :while t))

(defmethod dispatcher-send (form)
  (mp:process-send *process* form)
  (mp:process-poke *process*))

(defmethod dispatcher-add-operation ((operation operation))
  (dispatcher-send
   `(%add-operation ,operation))
  operation)

(defmethod dispatcher-remove-operation ((operation operation))
  (dispatcher-send
   `(%remove-operation ,operation)))


;;;;
;;;; Public interface
;;;;

(defmethod cancel ((operation operation))
  ;; return t if the operation was not yet cancelled
  (prog1
      (not (operation-cancelled-p operation))
    (dispatcher-remove-operation operation)))

(defun dispatch (&rest operation-initargs)
  (dispatcher-add-operation
   (apply #'make-instance 'operation
          operation-initargs)))

(defmethod dispatcher-running-p ()
  (mp:process-alive-p *process*))

(defmethod dispatcher-start ()
  (when (dispatcher-running-p)
    (error "DNSSD Dispatcher is already started."))

  #+win32 (fli:register-module "dnssd")
  (assert (> (daemon-version) 0))

  (setf *process*
        (mp:process-run-function "DNSSD Dispatcher"
                                 '(:mailbox t)
                                 #'dispatcher-loop))
  (values))

(defmethod dispatcher-stop ()
  (if (dispatcher-running-p)
      (progn
        (dispatcher-send #'(lambda ()
                             (mp:process-kill
                              (mp:get-current-process))))
        (mp:process-join *process*
                         :timeout +process-join-timeout+))
    (warn "DNSSD Dispatcher not running"))  
  (values))

(defmacro with-dispatcher (&body body)
  `(progn
     (unless (dispatcher-running-p)
       (dispatcher-start))
     (unwind-protect
         ,@body
       (dispatcher-stop))))
