#lang scheme/base

(require (planet untyped/unlib:3/symbol)
         "../base.ss"
         "../struct.ss")

; (struct expression boolean)
(define-struct formula (expr array?) #:transparent)

; (struct)
(define-struct expression () #:transparent)

; (struct symbol (listof expression))
(define-struct (operator expression) (name args) #:transparent)

; (struct symbol (listof expression))
(define-struct (function expression) (name args) #:transparent)

; (struct (listof expression))
(define-struct (array expression) (data) #:transparent)

; (struct any)
(define-struct (literal expression) (value) #:transparent)

; (struct cell boolean boolean)
(define-struct (cell-reference expression) (cell abs-x? abs-y?) #:transparent)

; Quoting ----------------------------------------

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
      (expression? item)
      (cell? item)
      (literal-value? item)))

; Provide statements -----------------------------

(provide/contract
 [struct formula                     ([expr expression?] [array? boolean?])]
 [struct expression                  ()]
 [struct (operator expression)       ([name symbol?] [args (listof expression?)])]
 [struct (function expression)       ([name symbol?] [args (listof expression?)])]
 [struct (array expression)          ([data (listof expression?)])]
 [struct (literal expression)        ([value literal-value?])]
 [struct (cell-reference expression) ([cell cell?] [abs-x? boolean?] [abs-y? boolean?])]
 [literal-value?                     (-> any/c boolean?)]
 [quotable?                          (-> any/c boolean?)])
