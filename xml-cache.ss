#lang scheme/base

(require "base.ss")

(require (unlib-in hash)
         "ref.ss"
         "struct.ss")

; (struct (hashof (list worksheet natural natural) (list cell style-id)
;         (hashof cell (list worksheet natural natural)))
;
; where style-id : natural
(define-struct cache (forward-lookup reverse-lookup) #:transparent)

; Constructor ------------------------------------

; -> cache
(define (create-cache)
  (make-cache (make-hash) (make-hasheq)))

; Helpers ----------------------------------------

; cache worksheet natural natural -> cell style-id
(define (cache-forward-ref cache sheet x y)
  (apply values (hash-ref (cache-forward-lookup cache)
                          (list sheet x y))))

; cache worksheet natural natural cell style-id -> void
(define (cache-forward-set! cache sheet x y cell style-id)
  (hash-set! (cache-forward-lookup cache)
             (list sheet x y)
             (list cell style-id)))

; cache cell -> worksheet natural natural
(define (cache-reverse-ref cache key)
  (apply values (hash-ref (cache-reverse-lookup cache) key)))

; cache cell (list worksheet natural natural) -> void
(define (cache-reverse-set! cache cell sheet x y)
  (hash-set! (cache-reverse-lookup cache)
             cell
             (list sheet x y)))

; Provide statements -----------------------------

(provide/contract
 [rename create-cache make-cache (-> cache?)]
 [cache?                         (-> any/c boolean?)]
 [cache-forward-ref              (-> cache?
                                     worksheet? 
                                     natural-number/c
                                     natural-number/c
                                     (values (or/c cell? #f)
                                             natural-number/c))]
 [cache-forward-set!             (-> cache?
                                     worksheet?
                                     natural-number/c
                                     natural-number/c
                                     (or/c cell? #f)
                                     natural-number/c
                                     void?)]
 [cache-reverse-ref              (-> cache?
                                     cell?
                                     (values worksheet?
                                             natural-number/c
                                             natural-number/c))]
 [cache-reverse-set!             (-> cache?
                                     cell?
                                     worksheet?
                                     natural-number/c
                                     natural-number/c
                                     void?)])