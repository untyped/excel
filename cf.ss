#lang scheme/base

(require "base.ss")

(require "formula-syntax.ss"
         "struct.ss")

; quotable compiled-style [natural] -> conditional-format
(define (cf= val style [priority 1])
  (make-conditional-format (fx (= (!this) ,val)) style priority))

; quotable compiled-style [natural] -> conditional-format
(define (cf<> val style [priority 1])
  (make-conditional-format (fx (<> (!this) ,val)) style priority))

; quotable compiled-style [natural] -> conditional-format
(define (cf> val style [priority 1])
  (make-conditional-format (fx (>  (!this) ,val)) style priority))

; quotable compiled-style [natural] -> conditional-format
(define (cf< val style [priority 1])
  (make-conditional-format (fx (<  (!this) ,val)) style priority))

; quotable compiled-style [natural] -> conditional-format
(define (cf>= val style [priority 1])
  (make-conditional-format (fx (>= (!this) ,val)) style priority))

; quotable compiled-style [natural] -> conditional-format
(define (cf<= val style [priority 1])
  (make-conditional-format (fx (<= (!this) ,val)) style priority))

; quotable compiled-style [natural] -> conditional-format
(define (cf-begins-with val style [priority 1])
  (make-conditional-format (fx (= (left (!this) (len ,val)) ,val)) style priority))

; quotable compiled-style [natural] -> conditional-format
(define (cf-ends-with val style [priority 1])
  (make-conditional-format (fx (= (right (!this) (len ,val)) ,val)) style priority))

; quotable compiled-style [natural] -> conditional-format
(define (cf-contains val style [priority 1])
  (make-conditional-format (fx (isnumber (search ,val (!this)))) style priority))

; quotable compiled-style [natural] -> conditional-format
(define (cf-not-contains val style [priority 1])
  (make-conditional-format (fx (iserror (search ,val (!this)))) style priority))

; quotable quotable compiled-style [natural] -> conditional-format
(define (cf-between val1 val2 style [priority 1])
  (make-conditional-format (fx (and (>= (!this) val1) (<= (!this) val2))) style priority))

; quotable quotable compiled-style [natural] -> conditional-format
(define (cf-not-between val1 val2 style [priority 1])
  (make-conditional-format (fx (or (< (!this) val1) (> (!this) val2))) style priority))

; quotable compiled-style [natural] -> conditional-format
(define (cf fx style [priority 1])
  (make-conditional-format (quote-formula fx) style priority))

; Provide statements -----------------------------

(provide/contract
 [cf=             (->* (quotable? compiled-style?) (natural-number/c) conditional-format?)]
 [cf<>            (->* (quotable? compiled-style?) (natural-number/c) conditional-format?)]
 [cf>             (->* (quotable? compiled-style?) (natural-number/c) conditional-format?)]
 [cf<             (->* (quotable? compiled-style?) (natural-number/c) conditional-format?)]
 [cf>=            (->* (quotable? compiled-style?) (natural-number/c) conditional-format?)]
 [cf<=            (->* (quotable? compiled-style?) (natural-number/c) conditional-format?)]
 [cf-begins-with  (->* (quotable? compiled-style?) (natural-number/c) conditional-format?)]
 [cf-ends-with    (->* (quotable? compiled-style?) (natural-number/c) conditional-format?)]
 [cf-contains     (->* (quotable? compiled-style?) (natural-number/c) conditional-format?)]
 [cf-not-contains (->* (quotable? compiled-style?) (natural-number/c) conditional-format?)]
 [cf-between      (->* (quotable? quotable? compiled-style?) (natural-number/c) conditional-format?)]
 [cf-not-between  (->* (quotable? quotable? compiled-style?) (natural-number/c) conditional-format?)]
 [cf              (->* (quotable? compiled-style?) (natural-number/c) conditional-format?)])