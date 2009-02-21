#lang scheme/base

(require "base.ss")


(require "struct.ss"
         "formula/formula.ss")

; Procedures -------------------------------------

; (U range quotable) ... [#:style style] -> union
(define (hc-append #:style [style empty-style] . ranges)
  (let* ([ranges         (map quote-range ranges)]
         [overall-width  (apply + (map range-width ranges))]
         [overall-height (apply max (map range-height ranges))])
    (make-union (compose ranges
                         (lambda (range x y) x)
                         (lambda (range x y) (quotient (- overall-height (range-height range)) 2))
                         (lambda (range x y) (range-width range))
                         (lambda (range x y) 0))
                overall-width
                overall-height
                style)))

; (U range quotable) ... [#:style style] -> union
(define (ht-append #:style [style empty-style] . ranges)
  (let* ([ranges         (map quote-range ranges)]
         [overall-width  (apply + (map range-width ranges))]
         [overall-height (apply max (map range-height ranges))])
    (make-union (compose ranges
                         (lambda (range x y) x)
                         (lambda (range x y) 0)
                         (lambda (range x y) (range-width range))
                         (lambda (range x y) 0))
                overall-width
                overall-height
                style)))

; (U range quotable) ... [#:style style] -> union
(define (hb-append #:style [style empty-style] . ranges)
  (let* ([ranges         (map quote-range ranges)]
         [overall-width  (apply + (map range-width ranges))]
         [overall-height (apply max (map range-height ranges))])
    (make-union (compose ranges
                         (lambda (range x y) x)
                         (lambda (range x y) (- overall-height (range-height range)))
                         (lambda (range x y) (range-width range))
                         (lambda (range x y) 0))
                overall-width
                overall-height
                style)))

; (U range quotable) ... [#:style style] -> union
(define (vc-append #:style [style empty-style] . ranges)
  (let* ([ranges         (map quote-range ranges)]
         [overall-width  (apply max (map range-width ranges))]
         [overall-height (apply + (map range-height ranges))])
    (make-union (compose ranges
                         (lambda (range x y) (quotient (- overall-width (range-width range)) 2))
                         (lambda (range x y) y)
                         (lambda (range x y) 0)
                         (lambda (range x y) (range-height range)))
                overall-width
                overall-height
                style)))

; (U range quotable) ... [#:style style] -> union
(define (vl-append #:style [style empty-style] . ranges)
  (let* ([ranges         (map quote-range ranges)]
         [overall-width  (apply max (map range-width ranges))]
         [overall-height (apply + (map range-height ranges))])
    (make-union (compose ranges
                         (lambda (range x y) 0)
                         (lambda (range x y) y)
                         (lambda (range x y) 0)
                         (lambda (range x y) (range-height range)))
                overall-width
                overall-height
                style)))

; (U range quotable) ... [#:style style] -> union
(define (vr-append #:style [style empty-style] . ranges)
  (let* ([ranges         (map quote-range ranges)]
         [overall-width  (apply max (map range-width ranges))]
         [overall-height (apply + (map range-height ranges))])
    (make-union (compose ranges
                         (lambda (range x y) (- overall-width (range-width range)))
                         (lambda (range x y) y)
                         (lambda (range x y) 0)
                         (lambda (range x y) (range-height range)))
                overall-width
                overall-height
                style)))

; (U range quotable) [natural] [#:style style] -> range
(define (l-pad range [num 1] #:style [style empty-style])
  (let ([range (quote-range range)])
    (make-union (list (make-part range num 0))
                (+ (range-width range) num)
                (range-height range))))

; (U range quotable) [natural] [#:style style] -> range
(define (r-pad range [num 1] #:style [style empty-style])
  (let ([range (quote-range range)])
    (make-union (list (make-part range 0 0))
                (+ (range-width range) num)
                (range-height range)
                style)))

; (U range quotable) [natural] [#:style style] -> range
(define (t-pad range [num 1] #:style [style empty-style])
  (let ([range (quote-range range)])
    (make-union (list (make-part range 0 num))
                (range-width range)
                (+ (range-height range) num)
                style)))

; (U range quotable) [natural] [#:style style] -> range
(define (b-pad range [num 1] #:style [style empty-style])
  (let ([range (quote-range range)])
    (make-union (list (make-part range 0 0))
                (range-width range)
                (+ (range-height range) num)
                style)))

; range style -> range
(define (style-range range style)
  (if style
      (make-union (list (make-part range 0 0))
                  (range-width range)
                  (range-height range)
                  style)
      range))

; range [border-style] [color] -> range
(define (outline-range range [border-style (border-style thin)] [color (rgb 0 0 0)])
  (let* ([w    (sub1 (range-width  range))]
         [h    (sub1 (range-height range))]
         [line (make-line border-style color)])
    (style-range range
                 (make-uncompiled-style
                  (lambda (x y)
                    (make-compiled-style #:border (make-border #:top    (and (= y 0) line)
                                                               #:right  (and (= x w) line)
                                                               #:bottom (and (= y h) line)
                                                               #:left   (and (= x 0) line))))))))

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
 [hc-append     (->* () (#:style style?) #:rest (listof range+quotable?) union?)]
 [ht-append     (->* () (#:style style?) #:rest (listof range+quotable?) union?)]
 [hb-append     (->* () (#:style style?) #:rest (listof range+quotable?) union?)]
 [vc-append     (->* () (#:style style?) #:rest (listof range+quotable?) union?)]
 [vl-append     (->* () (#:style style?) #:rest (listof range+quotable?) union?)]
 [vr-append     (->* () (#:style style?) #:rest (listof range+quotable?) union?)]
 [l-pad         (->* (range+quotable?) (natural-number/c #:style style?) range?)]
 [r-pad         (->* (range+quotable?) (natural-number/c #:style style?) range?)]
 [t-pad         (->* (range+quotable?) (natural-number/c #:style style?) range?)]
 [b-pad         (->* (range+quotable?) (natural-number/c #:style style?) range?)]
 [style-range   (-> range? style? range?)]
 [outline-range (->* (range?) (border-style? color?) range?)])