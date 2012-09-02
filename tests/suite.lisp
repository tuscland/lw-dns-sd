;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package #:zeroconf-tests)

(def-suite zeroconf)

(defun run-tests ()
  (eos:run! 'zeroconf))
