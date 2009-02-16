#lang scheme/base

(require "base.ss")

(require (unlib-in hash symbol)
         "ref.ss"
         "struct-internal.ss"
         "struct-style.ss")

; Workbook wrappers ------------------------------

; [#:id symbol] (listof worksheet) ->  workbook
(define (create-workbook #:id [id (gensym/interned 'book)] [sheets null])
  (make-workbook id sheets))

; Worksheet wrappers -----------------------------

;  [#:id symbol] 
;  string 
;  range
;  [#:protect? boolean]
;  [#:auto-filter-lock? boolean]
;  ...
; ->
;  worksheet
(define (create-worksheet #:id [id (gensym/interned 'sheet)]
                          name
                          data
                          #:auto-filter-lock?             [auto-filter-lock?             #f]
                          #:delete-columns-lock?          [delete-columns-lock?          #f]
                          #:delete-rows-lock?             [delete-rows-lock?             #f]
                          #:format-cells-lock?            [format-cells-lock?            #f]
                          #:format-columns-lock?          [format-columns-lock?          #f]
                          #:format-rows-lock?             [format-rows-lock?             #f]
                          #:insert-columns-lock?          [insert-columns-lock?          #f]
                          #:insert-hyperlinks-lock?       [insert-hyperlinks-lock?       #f]
                          #:insert-rows-lock?             [insert-rows-lock?             #f]
                          #:objects-lock?                 [objects-lock?                 #f]
                          #:pivot-tables-lock?            [pivot-tables-lock?            #f]
                          #:scenarios-lock?               [scenarios-lock?               #f]
                          #:locked-cell-selection-lock?   [locked-cell-selection-lock?   #f]
                          #:unlocked-cell-selection-lock? [unlocked-cell-selection-lock? #f]
                          #:sheet-lock?                   [sheet-lock?                   #f]
                          #:sort-lock?                    [sort-lock?                    #f])
  (make-worksheet id 
                  name
                  data
                  auto-filter-lock?
                  delete-columns-lock?
                  delete-rows-lock?
                  format-cells-lock?
                  format-columns-lock?
                  format-rows-lock?
                  insert-columns-lock?
                  insert-hyperlinks-lock?
                  insert-rows-lock?
                  objects-lock?
                  pivot-tables-lock?
                  scenarios-lock?
                  locked-cell-selection-lock?
                  unlocked-cell-selection-lock?
                  sheet-lock?
                  sort-lock?))

; Range wrappers ---------------------------------

; (listof part) natural natural [#:style style] -> union
(define (create-union parts x y [style empty-style])
  (make-union style parts x y))

; any [#:style style] -> cell
(define (create-cell val [style empty-style])
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
 [rename create-worksheet make-worksheet (->* (string? range?)
                                              (#:id symbol?
                                                    #:auto-filter-lock?             boolean?
                                                    #:delete-columns-lock?          boolean?
                                                    #:delete-rows-lock?             boolean?
                                                    #:format-cells-lock?            boolean?
                                                    #:format-columns-lock?          boolean?
                                                    #:format-rows-lock?             boolean?
                                                    #:insert-columns-lock?          boolean?
                                                    #:insert-hyperlinks-lock?       boolean?
                                                    #:insert-rows-lock?             boolean?
                                                    #:objects-lock?                 boolean?
                                                    #:pivot-tables-lock?            boolean?
                                                    #:scenarios-lock?               boolean?
                                                    #:locked-cell-selection-lock?   boolean?
                                                    #:unlocked-cell-selection-lock? boolean?
                                                    #:sheet-lock?                   boolean?
                                                    #:sort-lock?                    boolean?)
                                              worksheet?)]
 [rename create-union     make-union     (->* ((listof part?) natural-number/c natural-number/c) (style?) union?)]
 [rename create-cell      make-cell      (->* (any/c) (style?) cell?)]
 [range-children                         (-> range? (listof range?))]
 [range-width                            (-> range? natural-number/c)]
 [range-height                           (-> range? natural-number/c)])