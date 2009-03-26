#lang scheme/base

(require "base.ss")

(require (unlib-in hash profile)
         "ref.ss"
         "struct.ss")

(define-timer value-set-timer)
(define-timer value-ref-timer)
(define-timer address-set-timer)
(define-timer address-ref-timer)
(define-timer style-set-timer)
(define-timer style-ref-timer)
(define-timer diff-style-set-timer)
(define-timer diff-style-ref-timer)
(define-timer col-width-set-timer)
(define-timer col-width-ref-timer)
(define-timer row-height-set-timer)
(define-timer row-height-ref-timer)
(define-timer col-visibility-set-timer)
(define-timer col-visibility-ref-timer)
(define-timer row-visibility-set-timer)
(define-timer row-visibility-ref-timer)

; (struct (hashof (list worksheet natural natural) (list cell style-id)
;         (hashof cell (list worksheet natural natural))
;         (hashof (U cell-style number-format etc...) natural)
;         (hashof (U cell-style number-format etc...) natural)
;         (hashof (cons symbol natural) real)
;         (hashof (cons symbol natural) real)
;         (hashof (cons symbol natural) boolean)
;         (hashof (cons symbol natural) boolean))
;
; where style-id : natural
(define-struct cache
  (value-lookup
   address-lookup
   style-lookup
   diff-style-lookup
   col-width-lookup
   row-height-lookup
   col-visibility-lookup
   row-visibility-lookup)
  #:transparent)

; Constructor ------------------------------------

; -> cache
(define (create-cache)
  (make-cache (make-hash)
              (make-hasheq)
              (make-hash)
              (make-hash)
              (make-hash)
              (make-hash)
              (make-hash)
              (make-hash)))

; Helpers ----------------------------------------

; cache worksheet natural natural -> cell style-id
(define (cache-value-ref cache sheet x y)
  (with-timer value-ref-timer
    (apply values (hash-ref (cache-value-lookup cache)
                            (list (package-part-id sheet) x y)
                            (list #f #f)))))

; cache worksheet natural natural cell style-id -> void
(define (cache-value-set! cache sheet x y cell style-id)
  (with-timer value-set-timer
    (hash-set! (cache-value-lookup cache)
               (list (package-part-id sheet) x y)
               (list cell style-id))))

; cache range -> worksheet natural natural
(define (cache-address-ref cache key)
  (with-timer address-ref-timer
    (apply values (hash-ref (cache-address-lookup cache) key))))

; cache range (list worksheet natural natural) -> void
(define (cache-address-set! cache range sheet x y)
  (with-timer address-set-timer
    (hash-set! (cache-address-lookup cache) range (list sheet x y))))

; cache (U number-format font fill border compiled-style) [any] -> (U natural any)
(define cache-style-ref
  (with-timer style-ref-timer
    (case-lambda
      [(cache style)     (hash-ref (cache-style-lookup cache) style)]
      [(cache style def) (hash-ref (cache-style-lookup cache) style def)])))

; cache (U number-format font fill border compiled-style) natural -> void
(define (cache-style-set! cache style id)
  (with-timer style-set-timer
    (hash-set! (cache-style-lookup cache) style id)))

; cache compiled-style [any] -> (U natural any)
(define cache-diff-style-ref
  (with-timer diff-style-ref-timer
    (case-lambda
      [(cache style)     (hash-ref (cache-diff-style-lookup cache) style)]
      [(cache style def) (hash-ref (cache-diff-style-lookup cache) style def)])))

; cache ccompiled-style natural -> void
(define (cache-diff-style-set! cache style id)
  (with-timer diff-style-set-timer
    (hash-set! (cache-diff-style-lookup cache) style id)))

; cache worksheet natural -> (U real #f)
(define cache-col-width-ref
  (with-timer col-width-ref-timer
    (case-lambda
      [(cache sheet index)
       (let ([key (cons (package-part-id sheet) index)])
         (hash-ref (cache-col-width-lookup cache) key #f))])))

; cache worksheet natural real -> void
(define (cache-col-width-set! cache sheet index width)
  (with-timer col-width-set-timer
    (let ([key (cons (package-part-id sheet) index)])
      (hash-set! (cache-col-width-lookup cache) key width))))

; cache worksheet natural -> (U real #f)
(define cache-row-height-ref
  (with-timer row-height-ref-timer
    (case-lambda
      [(cache sheet index)
       (let ([key (cons (package-part-id sheet) index)])
         (hash-ref (cache-row-height-lookup cache) key #f))])))

; cache worksheet natural real -> void
(define (cache-row-height-set! cache sheet index height)
  (with-timer row-height-set-timer
    (let ([key (cons (package-part-id sheet) index)])
      (hash-set! (cache-row-height-lookup cache) key height))))

; cache worksheet natural -> boolean
(define cache-col-visibility-ref
  (with-timer col-visibility-ref-timer
    (case-lambda
      [(cache sheet index)
       (let ([key (cons (package-part-id sheet) index)])
         (hash-ref (cache-col-visibility-lookup cache) key #t))])))

; cache worksheet natural boolean -> void
(define (cache-col-visibility-set! cache sheet index visibility)
  (with-timer col-visibility-set-timer
    (let ([key (cons (package-part-id sheet) index)])
      (hash-set! (cache-col-visibility-lookup cache) key visibility))))

; cache worksheet natural -> boolean
(define cache-row-visibility-ref
  (with-timer row-visibility-ref-timer
    (case-lambda
      [(cache sheet index)
       (let ([key (cons (package-part-id sheet) index)])
         (hash-ref (cache-row-visibility-lookup cache) key #t))])))

; cache worksheet natural boolean -> void
(define (cache-row-visibility-set! cache sheet index visibility)
  (with-timer row-visibility-set-timer
    (let ([key (cons (package-part-id sheet) index)])
      (hash-set! (cache-row-visibility-lookup cache) key visibility))))

; Provide statements -----------------------------

; contract
(define style+component/c
  (or/c number-format? font? fill? border? compiled-style?))

(provide/contract
 [rename create-cache make-cache (->  cache?)]
 [cache?                         (->  any/c boolean?)]
 [cache-value-ref                (->  cache?
                                      worksheet? 
                                      natural-number/c
                                      natural-number/c
                                      (values (or/c cell? #f)
                                              (or/c natural-number/c #f)))]
 [cache-value-set!               (->  cache?
                                      worksheet?
                                      natural-number/c
                                      natural-number/c
                                      (or/c cell? #f)
                                      natural-number/c
                                      void?)]
 [cache-address-ref              (->  cache? range? (values worksheet? natural-number/c natural-number/c))]
 [cache-address-set!             (->  cache? range? worksheet? natural-number/c natural-number/c void?)]
 [cache-style-ref                (->* (cache? style+component/c) (any/c) any)]
 [cache-style-set!               (->  cache? style+component/c natural-number/c void?)]
 [cache-diff-style-ref           (->* (cache? compiled-style?) (any/c) any)]
 [cache-diff-style-set!          (->  cache? compiled-style? natural-number/c void?)]
 [cache-col-width-ref            (->  cache? worksheet? natural-number/c (or/c number? #f))]
 [cache-col-width-set!           (->  cache? worksheet? natural-number/c number? void?)]
 [cache-row-height-ref           (->  cache? worksheet? natural-number/c (or/c number? #f))]
 [cache-row-height-set!          (->  cache? worksheet? natural-number/c number? void?)]
 [cache-col-visibility-ref       (->  cache? worksheet? natural-number/c boolean?)]
 [cache-col-visibility-set!      (->  cache? worksheet? natural-number/c boolean? void?)]
 [cache-row-visibility-ref       (->  cache? worksheet? natural-number/c boolean?)]
 [cache-row-visibility-set!      (->  cache? worksheet? natural-number/c boolean? void?)])
