#lang scheme/base

(require scheme/contract
         (only-in (planet dherman/javascript:8/ast)
                  prefix-operators
                  postfix-operators
                  infix-operators
                  assignment-operators
                  prefix-operator?
                  postfix-operator?
                  infix-operator?
                  assignment-operator?))

; Operator symbols -------------------------------

; (listof symbol)
(define operator-names null)

; (hasheqof symbol string)
(define operator-lookup (make-hasheq))

; (hasheqof symbol (U natural #f))
(define arity-lookup (make-hasheq))

; (hasheqof symbol (U 'infix 'prefix 'postfix 'function))
(define type-lookup (make-hasheq))

; symbol (U natural #f) [(U 'infix 'prefix 'function)] [string] -> void
(define (register-operator! scheme arity [type 'infix] [excel (symbol->string scheme)])
  (set! operator-names `(,@operator-names ,scheme))
  (hash-set! operator-lookup scheme excel)
  (hash-set! type-lookup scheme type)
  (hash-set! arity-lookup scheme arity))

(register-operator! '+          #f)
(register-operator! '-          #f)
(register-operator! '*          #f)
(register-operator! '/          2)
(register-operator! '&          #f)
(register-operator! 'exp        2  'infix    "^")
;(register-operator! 'or         #f 'function)
;(register-operator! 'and        #f 'function)
;(register-operator! 'not        1  'function)
(register-operator! '=          2)
(register-operator! '>          2)
(register-operator! '<          2)
(register-operator! '>=         2)
(register-operator! '<=         2)
(register-operator! '<>         2)
(register-operator! '%          1  'postfix)
(register-operator! ':          2)
(register-operator! '!range-or  #f 'infix    ",")
(register-operator! '!range-and #f 'infix    " ")

; Predicates -------------------------------------

; symbol -> boolean
(define (operator-name? op)
  (and (memq op operator-names) #t))

; symbol -> string
(define (operator->string op)
  (hash-ref operator-lookup op))

; symbol -> (U natural #f)
(define (operator-arity op)
  (hash-ref arity-lookup op))

; symbol -> (U 'infix 'prefix 'postfix 'function)
(define (operator-type op)
  (hash-ref type-lookup op))

; symbol (listof any) -> boolean
(define (operator-arity-okay? op arity)
  (let ([op-arity (operator-arity op)])
    (or (not op-arity) (= arity op-arity))))

; Provide statements -----------------------------

(provide/contract
 [operator-names       (listof symbol?)]
 [operator-name?       (-> symbol? boolean?)]
 [operator->string     (-> symbol? string?)]
 [operator-arity       (-> symbol? (or/c natural-number/c #f))]
 [operator-type        (-> symbol? (or/c 'infix 'prefix 'postfix 'function))]
 [operator-arity-okay? (-> symbol? natural-number/c boolean?)])
