#lang scheme/base

(require "base.ss")

(require (unlib-in hash)
         "ref.ss"
         "struct.ss")

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
(define-struct cache (value-lookup
                      address-lookup
                      style-lookup
                      diff-style-lookup
                      col-width-lookup
                      row-height-lookup
                      col-visibility-lookup
                      row-visibility-lookup) #:transparent)

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
  (apply values (hash-ref (cache-value-lookup cache)
                          (list sheet x y)
                          (list #f #f))))

; cache worksheet natural natural cell style-id -> void
(define (cache-value-set! cache sheet x y cell style-id)
  (hash-set! (cache-value-lookup cache)
             (list sheet x y)
             (list cell style-id)))

; cache cell -> worksheet natural natural
(define (cache-address-ref cache key)
  (apply values (hash-ref (cache-address-lookup cache) key)))

; cache cell (list worksheet natural natural) -> void
(define (cache-address-set! cache cell sheet x y)
  (hash-set! (cache-address-lookup cache)
             cell
             (list sheet x y)))

; cache (U number-format font fill border compiled-style) [any] -> (U natural any)
(define cache-style-ref
  (case-lambda
    [(cache style)     (hash-ref (cache-style-lookup cache) style)]
    [(cache style def) (hash-ref (cache-style-lookup cache) style def)]))

; cache (U number-format font fill border compiled-style) natural -> void
(define (cache-style-set! cache style id)
  (hash-set! (cache-style-lookup cache) style id))

; cache compiled-style [any] -> (U natural any)
(define cache-diff-style-ref
  (case-lambda
    [(cache style)     (hash-ref (cache-diff-style-lookup cache) style)]
    [(cache style def) (hash-ref (cache-diff-style-lookup cache) style def)]))

; cache ccompiled-style natural -> void
(define (cache-diff-style-set! cache style id)
  (hash-set! (cache-diff-style-lookup cache) style id))

; cache worksheet natural -> (U real #f)
(define cache-col-width-ref
  (case-lambda
    [(cache sheet index)
     (let ([key (cons (package-part-id sheet) index)])
       (hash-ref (cache-col-width-lookup cache) key #f))]))

; cache worksheet natural real -> void
(define (cache-col-width-set! cache sheet index width)
  (let ([key (cons (package-part-id sheet) index)])
    (hash-set! (cache-col-width-lookup cache) key width)))

; cache worksheet natural -> (U real #f)
(define cache-row-height-ref
  (case-lambda
    [(cache sheet index)
     (let ([key (cons (package-part-id sheet) index)])
       (hash-ref (cache-row-height-lookup cache) key #f))]))

; cache worksheet natural real -> void
(define (cache-row-height-set! cache sheet index height)
  (let ([key (cons (package-part-id sheet) index)])
    (hash-set! (cache-row-height-lookup cache) key height)))

; cache worksheet natural -> boolean
(define cache-col-visibility-ref
  (case-lambda
    [(cache sheet index)
     (let ([key (cons (package-part-id sheet) index)])
       (hash-ref (cache-col-visibility-lookup cache) key #t))]))

; cache worksheet natural boolean -> void
(define (cache-col-visibility-set! cache sheet index visibility)
  (let ([key (cons (package-part-id sheet) index)])
    (hash-set! (cache-col-visibility-lookup cache) key visibility)))

; cache worksheet natural -> boolean
(define cache-row-visibility-ref
  (case-lambda
    [(cache sheet index)
     (let ([key (cons (package-part-id sheet) index)])
       (hash-ref (cache-row-visibility-lookup cache) key #t))]))

; cache worksheet natural boolean -> void
(define (cache-row-visibility-set! cache sheet index visibility)
  (let ([key (cons (package-part-id sheet) index)])
    (hash-set! (cache-row-visibility-lookup cache) key visibility)))

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
 [cache-address-ref              (->  cache?
                                      cell?
                                      (values worksheet?
                                              natural-number/c
                                              natural-number/c))]
 [cache-address-set!             (->  cache?
                                      cell?
                                      worksheet?
                                      natural-number/c
                                      natural-number/c
                                      void?)]
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
