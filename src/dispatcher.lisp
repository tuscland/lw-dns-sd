(defpackage #:com.wildora.dnssd.dispatcher
  (:import-from #:com.wildora.dnssd.operation
   #:operation
   #:operation-cancelled-p
   #:cancel-operation
   #:process-operation
   #:daemon-version))

(in-package #:com.wildora.dnssd.dispatcher)

(defglobal-variable *operations* nil)
(defglobal-variable *process* nil)
(defglobal-variable *cancel-operation-lock* (mp:make-lock :sharing t))
(defconstant +process-join-timeout+ 10)


(defun %add-operation (operation)
  (push operation *operations*))

(defun %remove-operation (operation &optional (removal-callback #'do-nothing))
  (setf *operations*
        (remove operation *operations*))
  (cancel-operation operation)
  (funcall removal-callback))

(defun %cleanup (process)
  (declare (ignore process))
  (dolist (operation (copy-seq *operations*))
    (%remove-operation operation)))

(defun dispatcher-wait-reason ()
  (format nil "Waiting, ~[no~:;~:*~D~] pending operation~:P"
          (length *operations*)))

(defun dispatcher-loop ()
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

(defun dispatcher-send (form)
  (assert (not (null *process*))
      ()
    "DNSSD Dispatcher not running")
  (mp:process-send *process* form)
  (mp:process-poke *process*))

(defun dispatcher-add-operation (operation)
  (dispatcher-send
   `(%add-operation ,operation))
  operation)

(defun dispatcher-remove-operation (operation removal-callback)
  (dispatcher-send
   `(%remove-operation ,operation ,removal-callback)))


;;;;
;;;; Public interface
;;;;

(define-condition cancel-timeout-error (dnssd-error)
  ()
  (:default-initargs
   :format-control "Waiting for operation event timed out"))

(defparameter *default-cancel-timeout* 60)

(defmethod cancel ((operation operation)
                   &key callback
                        (timeout *default-cancel-timeout*))
  (let (finished-waiting)
    (dispatcher-remove-operation operation
                                 (or callback
                                     #'(lambda ()
                                         (setf finished-waiting t))))
    (when (not (null callback))
      (mp:process-wait-with-timeout "Waiting for operation to be cancelled."
                                    timeout
                                    #'(lambda ()
                                        finished-waiting))))
  (values))

(defun dispatch (&rest operation-initargs)
  (dispatcher-add-operation
   (apply #'make-instance 'operation
          operation-initargs)))

(defun dispatcher-running-p ()
  (mp:process-alive-p *process*))

(defun dispatcher-start ()
  (when (dispatcher-running-p)
    (error "DNSSD Dispatcher is already started."))

  #+win32 (fli:register-module "dnssd")
  (assert (> (daemon-version) 0))

  (setf *process*
        (mp:process-run-function "DNSSD Dispatcher"
                                 '(:mailbox t)
                                 #'dispatcher-loop))
  (values))

(defun dispatcher-stop ()
  (if (dispatcher-running-p)
      (progn
        (dispatcher-send #'(lambda ()
                             (mp:process-kill
                              (mp:get-current-process))))
        (mp:process-join *process*
                         :timeout +process-join-timeout+)
        (setf *process* nil))
    (warn "DNSSD Dispatcher not running"))  
  (values))

(defmacro with-dispatcher (&body body)
  `(progn
     (unless (dispatcher-running-p)
       (dispatcher-start))
     (unwind-protect
         ,@body
       (dispatcher-stop))))
