(defpackage :zeroconf-user
  (:use :zeroconf :cl))

(in-package :zeroconf-user)


(defun service-equal (service1 service2)
  (string-equal (service-name service1)
                (service-name service2)))


(capi:define-interface browser (capi:interface
                                responder)
  ((browse-handle
    :initform nil
    :accessor browser-browse-handle)
   (services
    :initform nil
    :accessor browser-services))
  (:panes
   (services-list
    capi:list-panel
    :print-function 'service-name
    :reader browser-services-list)
   (run-button
    capi:button
    :selection-callback 'browser-run
    :reader browser-run-button)
   (spinner
    capi:cocoa-view-pane
    :view-class "NSProgressIndicator"
    :init-function 'browser-init-spinner
    :visible-max-height 32
    :reader browser-spinner))
  (:layouts
   (bottom-row
    capi:row-layout
    '(run-button spinner))
   (main
    capi:column-layout
    '(services-list bottom-row)
    :default t))
  (:default-initargs
   :callback-function 'browser-zeroconf-callback
   :error-function 'browser-zeroconf-error
   :create-callback 'browser-create
   :best-height 500
   :best-width 300
   :message-area t))

(defmethod browser-update-interface
           ((self browser))
  (let ((running (browser-running-p self)))
    (setf (capi:titled-object-message self)
          (if running
              "Running ..."
            "Stopped"))
    (setf (capi:item-text
           (browser-run-button self))
          (if running
              "Stop"
            "Run"))
    (objc:invoke
     (capi:cocoa-view-pane-view
      (browser-spinner self))
     (if running
         "startAnimation:"
       "stopAnimation:") nil)))

(defmethod browser-init-spinner (pane spinner)
  (declare (ignore pane))
  (setq spinner (objc:invoke spinner "init"))
  (objc:invoke spinner "setControlSize:"
               cocoa:ns-small-control-size)
  (objc:invoke spinner "setStyle:"
               cocoa:ns-progress-indicator-spinning-style)
  spinner)

(defmethod browser-create
           ((self browser))
  (browser-update-interface self))

(defmethod browser-running-p
           ((self browser))
  (not (null
        (browser-browse-handle self))))

(defmethod browser-start ((self browser))
  (setf (browser-browse-handle self)
        (browse self "_osc._udp")))

(defmethod browser-stop ((self browser))
  (cancel (browser-browse-handle self))
  (setf (browser-browse-handle self) nil))

(defun browser-run (data browser)
  (declare (ignore data))
  (if (browser-running-p browser)
      (browser-stop browser)
    (browser-start browser))
  (browser-update-interface browser))

(defmethod browser-handle-zeroconf-callback
           ((self browser) handle flags service)
  (when (member :add flags)
    (pushnew service
             (browser-services self)
             :test 'service-equal))
  (when (member :remove flags)
    (setf (browser-services self)
          (remove service
                  (browser-services self)
                  :test 'service-equal)))
  (when (member :finished flags)
    (setf (capi:collection-items
           (browser-services-list self))
          (browser-services self))))

(defmethod browser-handle-zeroconf-error
           ((self browser) handle condition)
  (setf (capi:titled-object-message self)
        (format nil "Error occured: ~A" condition))
  (browser-stop self))

(defmethod browser-zeroconf-error
           ((self browser) handle condition)
  (capi:execute-with-interface self
                               'browser-handle-zeroconf-error
                               self handle condition))

(defmethod browser-zeroconf-callback
           ((self browser) handle flags service)
  (capi:execute-with-interface self
                               'browser-handle-zeroconf-callback
                               self handle flags service))

(defun test-browser ()
  (capi:display (make-instance 'browser)))
