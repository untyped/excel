#lang scheme/base

(require (planet untyped/unlib:3/symbol)
         "../base.ss"
         "../struct.ss")

; (struct)
(define-struct formula () #:transparent)

; (struct symbol (listof expression)
(define-struct (operator formula) (name args) #:transparent)

; (struct symbol (listof expression)
(define-struct (function formula) (name args) #:transparent)

; (struct any)
(define-struct (literal formula) (value) #:transparent)

; (struct cell boolean boolean)
(define-struct (cell-reference formula) (cell abs-x? abs-y?) #:transparent)

; Constructors -----------------------------------

; literal-value -> literal
(define (create-literal val)
  (cond [(boolean? val)  (make-literal val)]
        [(integer? val)  (make-literal val)]
        [(real? val)     (make-literal val)]
        [(string? val)   (make-literal val)]
        [(symbol? val)   (make-literal val)]
        [else            (raise-exn exn:fail:contract
                           (format "Expected (U boolean integer real string symbol), received ~s" val))]))

; cell [boolean] [boolean] -> cell-reference
(define (create-cell-reference cell [abs-x? #f] [abs-y? #f])
  (make-cell-reference cell abs-x? abs-y?))

; Predicates -------------------------------------

; any -> boolean
(define (literal-value? item)
  (or (boolean? item)
      (integer? item)
      (real? item)
      (string? item)
      (symbol? item)))

; any -> boolean
(define (quotable? item)
  (or (formula? item)
      (cell? item)
      (literal-value? item)))

; (U formula cell literal-value) -> formula
(define (quote-argument arg)
  (cond [(formula? arg)       arg]
        [(cell? arg)          (create-cell-reference arg)]
        [(literal-value? arg) (create-literal arg)]
        [else (raise-exn exn:fail:contract
                (format "Expected (U formula boolean integer real string symbol), received ~s" arg))]))

; Provide statements -----------------------------

(provide/contract
 [struct formula                  ()]
 [struct (operator formula)       ([name symbol?] [args (listof quotable?)])]
 [struct (function formula)       ([name symbol?] [args (listof quotable?)])]
 [struct (literal formula)        ([value literal-value?])]
 [struct (cell-reference formula) ([cell cell?] [abs-x? boolean?] [abs-y? boolean?])]
 [create-literal                  (-> literal-value? literal?)]
 [create-cell-reference           (->* (cell?) (boolean? boolean?) cell-reference?)]
 [literal-value?                  (-> any/c boolean?)]
 [quotable?                       (-> any/c boolean?)]
 [quote-argument                  (-> quotable? formula?)])
