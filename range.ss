#lang scheme/base

(require "base.ss"
         "struct.ss"
         "formula/formula.ss")

; Procedures -------------------------------------

; (U range quotable) ... -> union
(define (hc-append . ranges)
  (let* ([ranges         (map quote-range ranges)]
         [overall-width  (apply + (map range-width ranges))]
         [overall-height (apply max (map range-height ranges))])
    (make-union (compose ranges
                         (lambda (range x y) x)
                         (lambda (range x y) (quotient (- overall-height (range-height range)) 2))
                         (lambda (range x y) (range-width range))
                         (lambda (range x y) 0))
                overall-width
                overall-height)))

; (U range quotable) ... -> union
(define (ht-append . ranges)
  (let* ([ranges         (map quote-range ranges)]
         [overall-width  (apply + (map range-width ranges))]
         [overall-height (apply max (map range-height ranges))])
    (make-union (compose ranges
                         (lambda (range x y) x)
                         (lambda (range x y) 0)
                         (lambda (range x y) (range-width range))
                         (lambda (range x y) 0))
                overall-width
                overall-height)))

; (U range quotable) ... -> union
(define (hb-append . ranges)
  (let* ([ranges         (map quote-range ranges)]
         [overall-width  (apply + (map range-width ranges))]
         [overall-height (apply max (map range-height ranges))])
    (make-union (compose ranges
                         (lambda (range x y) x)
                         (lambda (range x y) (- overall-height (range-height range)))
                         (lambda (range x y) (range-width range))
                         (lambda (range x y) 0))
                overall-width
                overall-height)))

; (U range quotable) ... -> union
(define (vc-append . ranges)
  (let* ([ranges         (map quote-range ranges)]
         [overall-width  (apply max (map range-width ranges))]
         [overall-height (apply + (map range-height ranges))])
    (make-union (compose ranges
                         (lambda (range x y) (quotient (- overall-width (range-width range)) 2))
                         (lambda (range x y) y)
                         (lambda (range x y) 0)
                         (lambda (range x y) (range-height range)))
                overall-width
                overall-height)))

; (U range quotable) ... -> union
(define (vl-append . ranges)
  (let* ([ranges         (map quote-range ranges)]
         [overall-width  (apply max (map range-width ranges))]
         [overall-height (apply + (map range-height ranges))])
    (make-union (compose ranges
                         (lambda (range x y) 0)
                         (lambda (range x y) y)
                         (lambda (range x y) 0)
                         (lambda (range x y) (range-height range)))
                overall-width
                overall-height)))

; (U range quotable) ... -> union
(define (vr-append . ranges)
  (let* ([ranges         (map quote-range ranges)]
         [overall-width  (apply max (map range-width ranges))]
         [overall-height (apply + (map range-height ranges))])
    (make-union (compose ranges
                         (lambda (range x y) (- overall-width (range-width range)))
                         (lambda (range x y) y)
                         (lambda (range x y) 0)
                         (lambda (range x y) (range-height range)))
                overall-width
                overall-height)))

; (U range quotable) [natural] -> range
(define (l-pad range [num 1])
  (let ([range (quote-range range)])
    (make-union (list (make-part range num 0))
                (+ (range-width range) num)
                (range-height range))))

; (U range quotable) [natural] -> range
(define (r-pad range [num 1])
  (let ([range (quote-range range)])
    (make-union (list (make-part range 0 0))
                (+ (range-width range) num)
                (range-height range))))

; (U range quotable) [natural] -> range
(define (t-pad range [num 1])
  (let ([range (quote-range range)])
    (make-union (list (make-part range 0 num))
                (range-width range)
                (+ (range-height range) num))))

; (U range quotable) [natural] -> range
(define (b-pad range [num 1])
  (let ([range (quote-range range)])
    (make-union (list (make-part range 0 0))
                (range-width range)
                (+ (range-height range) num))))

; Helpers ----------------------------------------

;  (listof range)
;  (range natural natural -> natural)
;  (range natural natural -> natural)
;  (range natural natural -> natural)
;  (range natural natural -> natural)
; ->
;  (listof part)
(define (compose ranges calc-x calc-y calc-dx calc-dy)
  (define-values (parts x y)
    (for/fold ([parts null] [x 0] [y 0])
              ([range (in-list ranges)])
              (values (cons (make-part range
                                       (calc-x range x y)
                                       (calc-y range x y))
                            parts)
                      (+ x (calc-dx range x y))
                      (+ y (calc-dy range x y)))))
  (reverse parts))

; any -> boolean
(define (range+quotable? item)
  (or (range? item)
      (boolean? item)
      (number? item)
      (symbol? item)
      (string? item)
      (formula? item)))

; (U range quotable) -> range
(define (quote-range item)
  (if (range? item)
      item
      (make-cell item)))

; Provide statements -----------------------------

(provide/contract
 [hc-append (->* () () #:rest (listof range+quotable?) union?)]
 [ht-append (->* () () #:rest (listof range+quotable?) union?)]
 [hb-append (->* () () #:rest (listof range+quotable?) union?)]
 [vc-append (->* () () #:rest (listof range+quotable?) union?)]
 [vl-append (->* () () #:rest (listof range+quotable?) union?)]
 [vr-append (->* () () #:rest (listof range+quotable?) union?)]
 [l-pad     (->* (range+quotable?) (natural-number/c) range?)]
 [r-pad     (->* (range+quotable?) (natural-number/c) range?)]
 [t-pad     (->* (range+quotable?) (natural-number/c) range?)]
 [b-pad     (->* (range+quotable?) (natural-number/c) range?)])