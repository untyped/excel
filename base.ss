#lang scheme/base

(require (planet untyped/unlib:3/require))

(define-library-aliases schemeunit (planet schematics/schemeunit:3) #:provide)
(define-library-aliases unlib      (planet untyped/unlib:3)         #:provide)

(require scheme/contract
         scheme/match
         scheme/path
         srfi/19
         srfi/26
         (planet untyped/mirrors:1)
         (planet untyped/unlib:3/debug)
         (planet untyped/unlib:3/exn))

; Configuration ----------------------------------

; (parameter (U natural #f))
(define max-function-arity         (make-parameter 29))
(define max-function-nesting-depth (make-parameter 8))

; Provide statements -----------------------------

(provide (all-from-out scheme/contract
                       scheme/match
                       scheme/path
                       srfi/19
                       srfi/26
                       (planet untyped/mirrors:1)
                       (planet untyped/unlib:3/debug)
                       (planet untyped/unlib:3/exn)))

(provide/contract
 [max-function-arity         (parameter/c (or/c natural-number/c #f))]
 [max-function-nesting-depth (parameter/c (or/c natural-number/c #f))])