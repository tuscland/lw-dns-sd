(in-package #:zeroconf)


(defun debugging-callback (prefix)
  #'(lambda (&rest args)
;;;; FIXME: why does *debug-io* not work as intended?
;;;; there is no output.
      (format t "~&~A: ~{~A~^, ~}~%" prefix args)))

(defparameter *debugging-enabled* nil)


(defun get-debug-collector ()
  (if *debugging-enabled*
      (capi:contain
       (make-instance 'capi:collector-pane))))

(defparameter *debug-collector* (get-debug-collector))

(defun get-debug-stream ()
  (if *debugging-enabled*
      (capi:collector-pane-stream *debug-collector*)))

(defparameter *debug-stream* (get-debug-stream))

(defun debug-print (&rest args)
  (when *debugging-enabled*
    (format *debug-stream* "~&~{~A ~}~%" args)))
