#lang scheme/base

(require scheme/port
         "../base.ss"
         "../ref.ss"
         "../struct.ss"
         "op.ss"
         "struct.ss")

; worksheet formula -> void
(define (formula->string sheet formula)
  (debug "fx" (with-output-to-string (cut display-formula sheet formula (current-output-port)))))

; Helpers ----------------------------------------
  
; worksheet formula output-port -> void
(define (display-formula sheet formula out)
  (match formula
    [(struct operator (name args))                (display-operator sheet name args out)]
    [(struct function (name args))                (display-function sheet name args out)]
    [(struct literal  (value))                    (display-literal sheet value out)]
    [(struct cell-reference (cell abs-x? abs-y?)) (display-cell-reference sheet cell abs-x? abs-y? out)]))

; worksheet symbol (listof formula) output-port -> void
(define (display-operator sheet name args out)
  (display-infix sheet (operator->string name) args out))

; worksheet symbol (listof formula) output-port -> void
(define (display-function sheet name args out)
  (display name out)
  (display-infix sheet "," args out))

; worksheet literal-value output-port -> void
(define (display-literal sheet val out)
  (match val
    [(? boolean?) (display (if val "TRUE" "FALSE") out)]
    [(? integer?) (display val out)]
    [(? real?)    (display val out)]
    [(? string?)  (display-string-literal sheet val out)]
    [(? symbol?)  (display-string-literal sheet (symbol->string val) out)]))

; worksheet string output-port -> void
(define (display-string-literal sheet str out)
  (display "\"" out)
  (display (regexp-replace* #rx"\"" str "\"\"") out)
  (display "\"" out))

; worksheet cell boolean boolean output-port -> void
(define (display-cell-reference sheet cell abs-x? abs-y? out)
  (let ([cell-sheet (cell-sheet cell)]
        [cell-x     (cell-x cell)]
        [cell-y     (cell-y cell)])
    (unless (and cell-sheet cell-x cell-y)
      (error "cell has not been added to a worksheet" cell))
    (if (eq? sheet cell-sheet)
        (display (xy->ref cell-x cell-y abs-x? abs-y?) out)
        (display (sheet+xy->ref cell-sheet cell-x cell-y abs-x? abs-y? out)))))

; worksheet (U symbol string) (listof formula) output-port -> void
; Wraps parentheses around the expression.
(define (display-infix sheet op args out)
  (display "(" out)
  (let loop ([first? #t] [args args])
    (unless (null? args)
      (unless first?
        (display op out))
      (display-formula sheet (car args) out)
      (loop #f (cdr args))))
  (display ")" out))

; Provide statements -----------------------------

(provide/contract
 [formula->string (-> worksheet? formula? string?)])
