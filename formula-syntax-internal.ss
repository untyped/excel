#lang scheme/base

(require "base.ss")

(require scheme/match
         (unlib-in syntax symbol)
         "formula-op.ss"
         "struct.ss"
         (for-template scheme/base
                       "struct.ss"))

; syntax -> syntax
(define (expand-expression stx)
  (syntax-case* stx (!ref !range !array !apply !cond !this quote unquote) symbolic-identifier=?
    [(!ref arg ...)              #`(make-cell-reference arg ...)]
    [(!this arg ...)             #`(make-this-reference arg ...)]
    [(!range arg ...)            #`(make-cell-range #,@(expand-args #'(arg ...)))]
    [(!array arg ...)            #`(make-array #,@(expand-args #'(arg ...)))]
    [(!apply op arg ...)         #`(apply #,@(syntax->list (expand-expression #'(op arg ...))))]
    [(!cond [else arg])          (eq? (syntax->datum #'else) 'else)
                                 (expand-expression #'arg)]
    [(!cond [test0 arg0]
            [test  arg] ...)     #`(if #,(expand-expression #'test0)
                                       #,(expand-expression #'arg0)
                                       #,(expand-expression #'(!cond [test arg] ...)))]
    [(!cond arg ...)             (raise-syntax-error #f "bad syntax" stx)]
    [(quote arg)                 #'(quote arg)]
    [(quote arg ...)             (raise-syntax-error #f "one argument only" stx)]
    [(unquote expr)              #'expr]
    [(unquote arg ...)           (raise-syntax-error #f "bad syntax" stx)]
    [(op arg ...)                (operator-name? (syntax->datum #'op))
                                 (if (operator-arity-okay? (syntax->datum #'op) (length (syntax->list #'(arg ...))))
                                     #`(make-operator 'op #,@(expand-args #'(arg ...)))
                                     (raise-syntax-error #f "wrong number of arguments" stx))]
    [(fn arg ...)                (identifier? #'fn)
                                 (if (function-name? (syntax->datum #'fn))
                                     #`(make-function 'fn #,@(expand-args #'(arg ...)))
                                     (raise-syntax-error #f "bad function name" stx #'fn))]
    [any                         (or (identifier? #'any) (literal-value? (syntax->datum #'any)))
                                 #`any]
    [_                           (error (format "bad syntax: ~s" (syntax->datum stx)))]))

; syntax -> (listof syntax)
(define (expand-args stx)
  (map expand-expression (syntax->list stx)))

; any -> boolean
(define (function-name? name)
  (and (symbol? name)
       (andmap char-alphabetic? (string->list (symbol->string name)))))

; Provide statements -----------------------------

(provide/contract
 [expand-expression (-> syntax? syntax?)])
