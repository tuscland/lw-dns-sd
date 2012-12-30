;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package #:com.wildora.dnssd-tests)

(def-suite dnssd)

(defun run-tests ()
  (eos:run! 'dnssd))
