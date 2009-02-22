#lang scheme/base

(require "base.ss")

(require "struct-style-internal.ss")

; (struct)
(define-struct data () #:transparent #:mutable)

; (struct symbol)
(define-struct (package-part data) (id) #:transparent #:mutable)

; (struct symbol (listof worksheet))
(define-struct (workbook package-part) (sheets) #:transparent #:mutable)

; (struct symbol (U string #f) range)
(define-struct (worksheet package-part)
  (name
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
   sort-lock?)
  #:transparent #:mutable)

; (struct style (listof conditional-format))
(define-struct (range data) (style conditional-formats) #:transparent #:mutable)

; (struct style (listof conditional-format) any)
(define-struct (cell range) (value) #:transparent #:mutable)

; (struct style (listof conditional-format) (listof part) natural natural)
(define-struct (union range) (parts width height) #:transparent)

; (struct range natural natural)
(define-struct (part data) (range dx dy) #:transparent)

; Provide statements -----------------------------

(provide/contract
 [struct data                     ()]
 [struct package-part             ([id                            symbol?])]
 [struct (workbook package-part)  ([id                            symbol?]
                                   [sheets                        (listof worksheet?)])]
 [struct (worksheet package-part) ([id                            symbol?]
                                   [name                          string?]
                                   [data                          range?]
                                   [auto-filter-lock?             boolean?]
                                   [delete-columns-lock?          boolean?]
                                   [delete-rows-lock?             boolean?]
                                   [format-cells-lock?            boolean?]
                                   [format-columns-lock?          boolean?]
                                   [format-rows-lock?             boolean?]
                                   [insert-columns-lock?          boolean?]
                                   [insert-hyperlinks-lock?       boolean?]
                                   [insert-rows-lock?             boolean?]
                                   [objects-lock?                 boolean?]
                                   [pivot-tables-lock?            boolean?]
                                   [scenarios-lock?               boolean?]
                                   [locked-cell-selection-lock?   boolean?]
                                   [unlocked-cell-selection-lock? boolean?]
                                   [sheet-lock?                   boolean?]
                                   [sort-lock?                    boolean?])]
 [struct (range data)             ([style                         style?]
                                   [conditional-formats           (listof conditional-format?)])]
 [struct (cell range)             ([style                         style?]
                                   [conditional-formats           (listof conditional-format?)]
                                   [value                         any/c])]
 [struct (union range)            ([style                         style?]
                                   [conditional-formats           (listof conditional-format?)]
                                   [parts                         (listof part?)]
                                   [width                         natural-number/c]
                                   [height                        natural-number/c])]
 [struct (part data)              ([range                         range?]
                                   [dx                            natural-number/c]
                                   [dy                            natural-number/c])])
