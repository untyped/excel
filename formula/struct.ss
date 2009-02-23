#lang scheme/base

(require "../base.ss"
         "../struct.ss"
         "struct-internal.ss")

; Wrappers ---------------------------------------

; (U expression quotable) [boolean] -> formula
(define (create-formula expr [array? #f])
  (make-formula (quote-expression expr) array?))

; symbol (U expression quotable) ... -> operator
(define (create-operator name . args)
  (make-operator name (map quote-expression args)))

; symbol (U expression quotable) ... -> function
(define (create-function name . args)
  (make-function name (map quote-expression args)))

; (U expression quotable) ... -> array
(define (create-array . args)
  (make-array (map quote-expression args)))

; literal-value -> literal
(define (create-literal val)
  (cond [(boolean? val) (make-literal val)]
        [(integer? val) (make-literal val)]
        [(real? val)    (make-literal val)]
        [(string? val)  (make-literal val)]
        [(symbol? val)  (make-literal val)]
        [else           (raise-exn exn:fail:contract
                          (format "Expected (U boolean integer real string symbol), received ~s" val))]))

; cell [boolean] [boolean] -> cell-reference
(define (create-cell-reference cell [abs-x? #f] [abs-y? #f])
  (make-cell-reference cell abs-x? abs-y?))

; Quoting ----------------------------------------

; (U formula expression cell literal-value) -> expression
(define (quote-expression arg)
  (cond [(formula? arg)       (formula-expr arg)]
        [(expression? arg)    arg]
        [(cell? arg)          (create-cell-reference arg)]
        [(literal-value? arg) (create-literal arg)]
        [else                 (raise-exn exn:fail:contract
                                (format "Expected (U expression boolean integer real string symbol), received ~s" arg))]))

; Provide statements -----------------------------

(provide (except-out (all-from-out "struct-internal.ss")
                     make-formula
                     make-operator
                     make-function
                     make-array
                     make-cell-reference
                     make-literal))

(provide/contract
 [rename create-formula        make-formula        (->* (quotable?) (boolean?) formula?)]
 [rename create-operator       make-operator       (->* (symbol?) () #:rest (listof quotable?) operator?)]
 [rename create-function       make-function       (->* (symbol?) () #:rest (listof quotable?) function?)]
 [rename create-array          make-array          (->* () () #:rest (listof quotable?) array?)]
 [rename create-literal        make-literal        (-> literal-value? literal?)]
 [rename create-cell-reference make-cell-reference (->* (cell?) (boolean? boolean?) cell-reference?)]
 [quote-expression                                 (-> quotable? expression?)])