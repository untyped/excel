#lang scheme/base

(require "test-base.ss")

(require (unlib-in hash)
         "xml-cache.ss"
         "range.ss"
         "struct.ss")

; Tests ------------------------------------------

(define cache-tests
  (test-suite "xml-cache.ss"))

; Provide statements -----------------------------

(provide cache-tests)