#lang scheme/base

(require (for-syntax scheme/base
                     "syntax-internal.ss")
         "struct.ss")

(define-syntax (fx stx)
  (syntax-case stx ()
    [(_ arg) #`(make-formula #,(expand-expression #'arg) #f)]))

(define-syntax (fx* stx)
  (syntax-case stx ()
    [(_ arg) #`(make-formula #,(expand-expression #'arg) #t)]))

; Provide statements -----------------------------

(provide fx fx*)
