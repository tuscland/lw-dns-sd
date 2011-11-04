(in-package #:zeroconf)


(defparameter *debugging-enabled* t)


(defun get-debug-collector ()
  (if *debugging-enabled*
      (capi:contain
       (make-instance 'capi:collector-pane))))

(defun get-debug-stream ()
  (if *debugging-enabled*
      (capi:collector-pane-stream *debug-collector*)))

(defvar *debug-collector* (get-debug-collector))
(defparameter *debug-stream* (get-debug-stream))

(defun debug-print (&rest args)
  (when *debugging-enabled*
    (format *debug-stream* "~&~{~A ~}~%" args)))
