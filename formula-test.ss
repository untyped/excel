#lang scheme/base

(require "test-base.ss")

(require "formula-render.ss"
         "formula-syntax.ss"
         "struct.ss"
         "xml-cache.ss")

(define (fx->string fx)
  (expression->string 
   (make-cache)
   (make-worksheet "Sheet1" (make-cell 123))
   (make-cell 123)
   0
   0
   (formula-expr fx)))

(define formula-tests
  (test-suite "formula.ss"
    (test-equal? "or"    (fx->string (fx (or 1 2 3)))   "OR(1,2,3)")
    (test-equal? "not"   (fx->string (fx (not 1)))      "NOT(1)")))

; Provide statements -----------------------------

(provide formula-tests)