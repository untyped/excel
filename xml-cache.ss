#lang scheme/base

(require "base.ss")

(require (unlib-in hash)
         "ref.ss"
         "struct.ss")

; (struct (hashof (list worksheet natural natural) (list cell style-id)
;         (hashof cell (list worksheet natural natural))
;         (hash
;
; where style-id : natural
(define-struct cache (value-lookup address-lookup style-lookup diff-style-lookup) #:transparent)

; Constructor ------------------------------------

; -> cache
(define (create-cache)
  (make-cache (make-hash) (make-hasheq) (make-hash) (make-hash)))

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

; Provide statements -----------------------------

; contract
(define style+component/c
  (or/c number-format? font? fill? border? compiled-style?))

(provide/contract
 [rename create-cache make-cache (-> cache?)]
 [cache?                         (-> any/c boolean?)]
 [cache-value-ref                (-> cache?
                                     worksheet? 
                                     natural-number/c
                                     natural-number/c
                                     (values (or/c cell? #f)
                                             (or/c natural-number/c #f)))]
 [cache-value-set!               (-> cache?
                                     worksheet?
                                     natural-number/c
                                     natural-number/c
                                     (or/c cell? #f)
                                     natural-number/c
                                     void?)]
 [cache-address-ref              (-> cache?
                                     cell?
                                     (values worksheet?
                                             natural-number/c
                                             natural-number/c))]
 [cache-address-set!             (-> cache?
                                     cell?
                                     worksheet?
                                     natural-number/c
                                     natural-number/c
                                     void?)]
 [cache-style-ref                (->* (cache? style+component/c)
                                      (any/c)
                                      any)]
 [cache-style-set!               (-> cache?
                                     style+component/c
                                     natural-number/c
                                     void?)]
 [cache-diff-style-ref           (->* (cache? compiled-style?)
                                      (any/c)
                                      any)]
 [cache-diff-style-set!          (-> cache?
                                     compiled-style?
                                     natural-number/c
                                     void?)])
