#lang scheme/base

(require scheme/port
         "../base.ss"
         "../xml-cache.ss"
         "../ref.ss"
         "../struct.ss"
         "op.ss"
         "struct.ss")

; cache worksheet expression -> string
(define (expression->string cache sheet expr)
  (with-output-to-string
   (cut display-expression cache sheet expr (current-output-port))))

; Helpers ----------------------------------------

; cache worksheet expression output-port -> void
(define (display-expression cache sheet expr out)
  (match expr
    [(struct operator (name args))                (display-operator cache sheet name args out)]
    [(struct function (name args))                (display-function cache sheet name args out)]
    [(struct literal  (value))                    (display-literal cache sheet value out)]
    [(struct array    (data))                     (display-array cache sheet data out)]
    [(struct cell-reference (cell abs-x? abs-y?)) (display-cell-reference cache sheet cell abs-x? abs-y? out)]))

; cache worksheet symbol (listof expression) output-port -> void
(define (display-operator cache sheet name args out)
  (case name
    [(%)  (display-postfix cache sheet (operator->string name) args out)]
    [else (display-infix cache sheet (operator->string name) args out)]))

; cache worksheet symbol (listof expression) output-port -> void
(define (display-function cache sheet name args out)
  (display (string-upcase (symbol->string name)) out)
  (display-infix cache sheet "," args out))

; cache worksheet literal-value output-port -> void
(define (display-literal cache sheet val out)
  (match val
    [(? boolean?) (display (if val "true" "false") out)]
    [(? integer?) (display val out)]
    [(? real?)    (display val out)]
    [(? string?)  (display-string-literal cache sheet val out)]
    [(? symbol?)  (display-string-literal cache sheet (symbol->string val) out)]))

; cache worksheet string output-port -> void
(define (display-string-literal cache sheet str out)
  (display "\"" out)
  (display (regexp-replace* #rx"\"" str "\"\"") out)
  (display "\"" out))

; cache worksheet (listof expression) output-port -> void
(define (display-array cache sheet data out)
  (display-infix cache sheet "," data out "{" "}"))

; cache worksheet cell boolean boolean output-port -> void
(define (display-cell-reference cache sheet cell abs-x? abs-y? out)
  (let-values ([(other-sheet x y) (cache-reverse-ref cache cell)])
    (if (eq? sheet other-sheet)
        (display (xy->ref                   x y abs-x? abs-y?) out)
        (display (sheet+xy->ref other-sheet x y abs-x? abs-y?) out))))

; cache worksheet (U symbol string) (listof expression) output-port [string] [string] -> void
; Wraps parentheses around the expression.
(define (display-infix cache sheet op args out [opening-bracket "("] [closing-bracket ")"])
  (display opening-bracket out)
  (let loop ([first? #t] [args args])
    (unless (null? args)
      (unless first?
        (display op out))
      (display-expression cache sheet (car args) out)
      (loop #f (cdr args))))
  (display closing-bracket out))

; cache worksheet (U symbol string) (listof expression) output-port -> void
(define (display-postfix cache sheet op args out)
  (display-expression cache sheet (car args) out)
  (display op out))

; Provide statements -----------------------------

(provide/contract
 [expression->string (-> cache? worksheet? expression? string?)])
