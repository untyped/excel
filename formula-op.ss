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

; symbol (U natural #f) [symbol] -> void
(define (register-operator! scheme arity [excel (symbol->string scheme)])
  (set! operator-names `(,@operator-names ,scheme))
  (hash-set! operator-lookup scheme excel)
  (hash-set! arity-lookup scheme arity))

(register-operator! '+ #f)
(register-operator! '- #f)
(register-operator! '* #f)
(register-operator! '/ 2)
(register-operator! '& #f)
(register-operator! 'exp 2 "^")
(register-operator! '= 2)
(register-operator! '> 2)
(register-operator! '< 2)
(register-operator! '>= 2)
(register-operator! '<= 2)
(register-operator! '<> 2)
(register-operator! '% 1)
(register-operator! ': 2)
(register-operator! '!range 2 ":")
(register-operator! '!range-or #f ",")
(register-operator! '!range-and #f " ")

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

; symbol (listof any) -> boolean
(define (operator-arity-okay? op arity)
  (let ([op-arity (operator-arity op)])
    (or (not op-arity) (= arity op-arity))))

; Provide statements -----------------------------

(provide/contract
 [operator-names       (listof symbol?)]
 [operator-name?       (-> symbol? boolean?)]
 [operator->string     (-> symbol? string?)]
 [operator-arity       (-> any/c (or/c natural-number/c #f))]
 [operator-arity-okay? (-> symbol? natural-number/c boolean?)])
