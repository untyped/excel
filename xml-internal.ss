#lang scheme/base

(require "base.ss")

; -> (-> natural)
(define (make-counter initial)
  (let ([counter initial])
    (lambda ()
      (begin0 counter
              (set! counter (add1 counter))))))

; Provide statements -----------------------------

(provide/contract
 [make-counter  (-> natural-number/c (-> natural-number/c))])

