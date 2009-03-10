#lang scheme/base

(require scheme/port
         "base.ss"
         "formula-op.ss"
         "ref.ss"
         "struct.ss"
         "xml-cache.ss")

; cache worksheet range natural natural expression -> string
(define (expression->string cache sheet range x y expr)
  (with-output-to-string
   (cut display-expression cache sheet range x y expr (current-output-port))))

; Helpers ----------------------------------------

; cache worksheet range natural natural expression output-port -> void
(define (display-expression cache sheet range x y expr out)
  (match expr
    [(struct operator (name args))                (display-operator cache sheet range x y name args out)]
    [(struct function (name args))                (display-function cache sheet range x y name args out)]
    [(struct literal  (value))                    (display-literal cache sheet range x y value out)]
    [(struct array    (data))                     (display-array cache sheet range x y data out)]
    [(struct range-reference (cell abs-x? abs-y?)) (display-range-reference cache sheet range x y cell abs-x? abs-y? out)]
    [(struct this-reference ())                   (display-this-reference cache sheet range x y out)]))

; cache worksheet range natural natural symbol (listof expression) output-port -> void
(define (display-operator cache sheet range x y name args out)
  (match (operator-type name)
    ['prefix   (display-prefix   cache sheet range x y (operator->string name) args out)]
    ['postfix  (display-postfix  cache sheet range x y (operator->string name) args out)]
    ['infix    (display-infix    cache sheet range x y (operator->string name) args out)]
    ['function (display-function cache sheet range x y (operator->string name) args out)]))

; cache worksheet range natural natural symbol (listof expression) output-port -> void
(define (display-function cache sheet range x y name args out)
  (display (string-upcase (if (string? name)
                              name
                              (symbol->string name))) out)
  (display-infix cache sheet range x y "," args out))

; cache worksheet range natural natural literal-value output-port -> void
(define (display-literal cache sheet range x y val out)
  (match val
    [(? boolean?) (display (if val "true" "false") out)]
    [(? integer?) (display val out)]
    [(? real?)    (display val out)]
    [(? string?)  (display-string-literal cache sheet range x y val out)]
    [(? symbol?)  (display-string-literal cache sheet range x y (symbol->string val) out)]))

; cache worksheet range natural natural string output-port -> void
(define (display-string-literal cache sheet range x y str out)
  (display "\"" out)
  (display (regexp-replace* #rx"\"" str "\"\"") out)
  (display "\"" out))

; cache worksheet range natural natural (listof expression) output-port -> void
(define (display-array cache sheet range x y data out)
  (display-infix cache sheet range x y "," data out "{" "}"))

; cache worksheet range natural natural cell boolean boolean output-port -> void
(define (display-range-reference cache sheet my-range my-x my-y range abs-x? abs-y? out)
  (let-values ([(other-sheet x y) (cache-address-ref cache range)])
    (if (eq? sheet other-sheet)
        (display (range-address range x y) out)
        (display (range-address range other-sheet x y) out))))

; cache worksheet range natural natural output-port -> void
(define (display-this-reference cache sheet range x y out)
  (let ([w (range-width range)]
        [h (range-height range)])
    (display (xy->ref x y) out)
    (when (or (> w 1) (> h 1))
      (display #\: out)
      (display (xy->ref (sub1 (+ x w)) (sub1 (+ y h))) out))))

; cache worksheet range natural natural (U symbol string) (listof expression) output-port [string] [string] -> void
; Wraps parentheses around the expression.
(define (display-infix cache sheet range x y op args out [opening-bracket "("] [closing-bracket ")"])
  (display opening-bracket out)
  (let loop ([first? #t] [args args])
    (unless (null? args)
      (unless first?
        (display op out))
      (display-expression cache sheet range x y (car args) out)
      (loop #f (cdr args))))
  (display closing-bracket out))

; cache worksheet range natural natural (U symbol string) (listof expression) output-port -> void
(define (display-prefix cache sheet range x y op args out)
  (display op out)
  (display-expression cache sheet range x y (car args) out))

; cache worksheet range natural natural (U symbol string) (listof expression) output-port -> void
(define (display-postfix cache sheet range x y op args out)
  (display-expression cache sheet range x y (car args) out)
  (display op out))

; Provide statements -----------------------------

(provide/contract
 [expression->string (-> cache? worksheet? range? natural-number/c natural-number/c expression? string?)])
