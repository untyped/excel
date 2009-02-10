#lang scheme/base

(require (planet untyped/unlib:3/hash)
         (planet untyped/unlib:3/symbol)
         "base.ss"
         "ref.ss"
         "struct-internal.ss"
         "struct-style.ss")

; Workbook wrappers ------------------------------

; [#:id symbol] (listof worksheet) ->  workbook
(define (create-workbook #:id [id (gensym/interned 'book)] [sheets null])
  (make-workbook id sheets))

; Worksheet wrappers -----------------------------

; [#:id symbol] string range -> worksheet
(define (create-worksheet #:id [id (gensym/interned 'sheet)] name data)
  (make-worksheet id name data))
  
; Range wrappers ---------------------------------

; (listof part) natural natural [#:style style] -> union
(define (create-union parts x y [style #f])
  (make-union style parts x y))

; any [#:style (U style #f)] -> cell
(define (create-cell val [style #f])
  (make-cell style val))

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

; range -> (listof range)
(define (range-children range)
  (if (cell? range)
      null
      (for/list ([part (in-list (union-parts range))])
        (part-range part))))

; Provide statements -----------------------------

(provide (except-out (all-from-out "struct-internal.ss")
                     make-workbook
                     make-worksheet
                     make-union
                     make-cell)
         (all-from-out "struct-style.ss"))

(provide/contract
 [rename create-workbook  make-workbook  (->* () (#:id symbol? (listof worksheet?)) workbook?)]
 [rename create-worksheet make-worksheet (->* (string? range?) (#:id symbol?) worksheet?)]
 [rename create-union     make-union     (->* ((listof part?) natural-number/c natural-number/c) ((or/c style? #f)) union?)]
 [rename create-cell      make-cell      (->* (any/c) ((or/c style? #f)) cell?)]
 [range-children                         (-> range? (listof range?))]
 [range-width                            (-> range? natural-number/c)]
 [range-height                           (-> range? natural-number/c)])