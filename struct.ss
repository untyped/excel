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

; Styles -----------------------------------------

; style -> boolean
(define (style-empty? style)
  (and (not (style-number-format style))))

; style (U style #f) -> style
(define (style-compose style1 style2)
  (if style2
      (make-style (or (style-number-format style2)
                      (style-number-format style1)))
      style1))

; Built-in styles and style components -----------

(define general-number-format (make-number-format #f))

; Provide statements -----------------------------

(provide (except-out (all-from-out "struct-internal.ss")
                     make-workbook
                     make-worksheet
                     make-union
                     make-cell))

(define nat/c natural-number/c)

(provide/contract
 [rename create-workbook  make-workbook  (->* () (#:id symbol? (listof worksheet?)) workbook?)]
 [rename create-worksheet make-worksheet (->* (string? range?) (#:id symbol?) worksheet?)]
 [rename create-union     make-union     (->* ((listof part?) nat/c nat/c) ((or/c style? #f)) union?)]
 [rename create-cell      make-cell      (->* (any/c) ((or/c style? #f)) cell?)]
 [range-children                         (-> range? (listof range?))]
 [range-width                            (-> range? nat/c)]
 [range-height                           (-> range? nat/c)]
 [style-empty?                           (-> style? boolean?)]
 [style-compose                          (-> style? (or/c style? #f) style?)]
 [general-number-format                  number-format?])