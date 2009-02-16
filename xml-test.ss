#lang scheme/base

(require "test-base.ss")

(require "xml.ss")

(define xml-tests
  (test-suite "xml.ss"))

; Provide statements -----------------------------

(provide xml-tests)