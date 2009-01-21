#lang scheme/base

(require (for-syntax scheme/base
                     "syntax-internal.ss"))

(define-syntax (fx stx)
  (syntax-case stx ()
    [(_ arg) #`(begin #,(expand-formula #'arg))]))

; Provide statements -----------------------------

(provide fx)
