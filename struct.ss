#lang scheme/base

(require (planet untyped/unlib:3/hash)
         (planet untyped/unlib:3/symbol)
         "base.ss"
         "ref.ss"
         "struct-internal.ss")

; Workbook wrappers ------------------------------

; [#:id symbol] (listof worksheet) ->  workbook
(define (create-workbook #:id [id (gensym/interned 'book)] [sheets null])
  (make-workbook id sheets))

; Worksheet wrappers -----------------------------

; [#:id symbol] string range -> worksheet
(define (create-worksheet #:id [id (gensym/interned 'sheet)] name data)
  (make-worksheet id name data))
  
; Range wrappers ---------------------------------

; range -> worksheet
#;(define (range-sheet range)
  (let ([parent (data-parent range)])
    (cond [(not parent)        (error "range does not have a parent" range)]
          [(worksheet? parent) parent]
          [else                (range-sheet parent)])))

; range -> natural
#;(define (range-x range [accum 0])
  (let ([parent (data-parent range)])
    (cond [(not parent)        (error "range does not have a parent" range)]
          [(worksheet? parent) accum]
          [(part? parent)      (range-x parent (+ accum (part-dx parent)))]
          [else                (range-x parent accum)])))

; range -> natural
#;(define (range-y range [accum 0])
  (let ([parent (data-parent range)])
    (cond [(not parent)        (error "range does not have a parent" range)]
          [(worksheet? parent) accum]
          [(part? parent)      (range-y parent (+ accum (part-dy parent)))]
          [else                (range-y parent accum)])))

; range -> natural
(define (range-width range)
  (if (union? range)
      (union-width range)
      1))

; range -> natural
(define (range-height range)
  (if (union? range)
      (union-height range)
      1))

; Provide statements -----------------------------

(provide (except-out (all-from-out "struct-internal.ss")
                     make-workbook
                     make-worksheet))

(define nat/c natural-number/c)

(provide/contract
 [rename create-workbook  make-workbook  (->* () (#:id symbol? (listof worksheet?)) workbook?)]
 [rename create-worksheet make-worksheet (->* (string? range?) (#:id symbol?) worksheet?)]
 #;[range-sheet                            (-> range? worksheet?)]
 #;[range-x                                (-> range? nat/c)]
 #;[range-y                                (-> range? nat/c)]
 [range-width                            (-> range? nat/c)]
 [range-height                           (-> range? nat/c)])