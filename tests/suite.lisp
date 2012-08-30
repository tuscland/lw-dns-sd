;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package #:zeroconf-tests)

(def-suite zeroconf)

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :zeroconf-tests))))
  (eos:run! 'zeroconf))
