#lang scheme/base

(require scheme/match
         (planet untyped/unlib:3/syntax)
         (planet untyped/unlib:3/symbol)
         "../base.ss"
         "op.ss"
         "struct.ss"
         (for-template scheme/base
                       "struct.ss"))

; syntax -> syntax
(define (expand-formula stx)
  (syntax-case* stx (!ref !range quote unquote) symbolic-identifier=?
    [(!ref arg ...)                    #`(make-cell-reference arg ...)]
    [(!range arg ...)                  #`(make-cell-range #,@(expand-args #'(arg ...)))]
    [(unquote expr)                    #`(quote-formula expr)]
    [(unquote arg ...)                 (raise-syntax-error #f "bad syntax" stx)]
    ; Operators:
    [(op arg ...)                  (operator-name? (syntax->datum #'op))
                                   (if (operator-arity-okay? (syntax->datum #'op) (length (syntax->list #'(arg ...))))
                                       #`(make-operator 'op (list #,@(expand-args #'(arg ...))))
                                       (raise-syntax-error #f "wrong number of arguments" stx))]
    [(quote arg)                   #'(quote arg)]
    [(quote arg ...)               (raise-syntax-error #f "one argument only" stx)]
    [(fn arg ...)                  (identifier? #'fn)
                                   #`(make-function 'fn (list #,@(expand-args #'(arg ...))))]
    [any                           (or (identifier? #'any) (literal-value? (syntax->datum #'any)))
                                   #`(quote-argument any)]
    [_                             (error (format "bad syntax: ~s" (syntax->datum stx)))]))

; syntax -> (listof syntax)
(define (expand-args stx)
  (map expand-formula (syntax->list stx)))

; Provide statements -----------------------------

(provide/contract
 [expand-formula (-> syntax? syntax?)])
