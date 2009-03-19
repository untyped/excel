#lang scheme/base

(require "base.ss")

(require (for-syntax scheme/base
                     scheme/match)
         (unlib-in string)
         "struct-style-internal.ss")

; (struct)
(define-struct data () #:transparent)

; (struct symbol)
(define-struct (package-part data) (id) #:transparent)

; (struct symbol (listof worksheet))
(define-struct (workbook package-part) (sheets) #:transparent)

; (struct symbol
;         (U string #f) 
;         range
;         (U split #f)
;         (U auto-filter #f)
;         (U print-settings #f)
;         boolean
;         ...)
(define-struct (worksheet package-part)
  (name
   data
   split
   auto-filter
   print-settings
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
  #:transparent)

; (struct (U range (cons natural natural))
;         (U range (cons natural natural) #f)
;         boolean)
(define-struct split (position scroll-position frozen?) #:transparent)

; (struct natural natural natural natural)
(define-struct auto-filter (x y width height) #:transparent)

; (struct (U natural #f)
;         (U natural #f)
;         (U 'portrait 'landscape)
;         (U string #f)
;         (U string #f))
(define-struct print-settings
  (fit-to-width fit-to-height orientation headers footers)
  #:transparent)

; (struct style (U validation-rule #f) (listof conditional-format))
(define-struct (range data) (style validation-rule conditional-formats) #:transparent)

; (struct style (U validation-rule #f) (listof conditional-format) any (U cell-dimensions #f))
(define-struct (cell range)
  (value dimensions)
  #:transparent
  #:property prop:custom-write
  (lambda (cell out write?)
    (define show (if write? write display))
    (display "#(struct:cell " out)
    (show (cell-value cell) out)
    (display ")" out)))

; (struct (U number #f) (U number #f) (U number #f) (U number #f) boolean boolean)
(define-struct cell-dims
  (min-width max-width min-height max-height hide-row? hide-column?) 
  #:transparent)

; (struct style (U validation-rule #f) (listof conditional-format) (listof part) natural natural)
(define-struct (union range) (parts width height) #:transparent)

; (struct range natural natural)
(define-struct (part data) (range dx dy) #:transparent)

; Core wrappers ----------------------------------

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

; part integer integer -> boolean
;
; Coordinates are in the coordinate system of part's parent range.
(define (part-contains? part x y)
  (let* ([x0 (part-dx part)]
         [y0 (part-dy part)]
         [x1 (+ x0 (range-width (part-range part)))]
         [y1 (+ y0 (range-height (part-range part)))])
    (and (>= x x0)
         (>= y y0)
         (< x x1)
         (< y y1))))

; Formulae and expressions -----------------------

; (struct expression boolean)
(define-struct formula (expr array?) #:transparent)

; (struct)
(define-struct expression () #:transparent)

; (struct symbol (listof expression))
(define-struct (operator expression) (name args) #:transparent)

; (struct symbol (listof expression))
(define-struct (function expression) (name args) #:transparent)

; (struct (listof expression))
(define-struct (array expression) (data) #:transparent)

; (struct any)
(define-struct (literal expression) (value) #:transparent)

; (struct cell boolean boolean boolean boolean)
(define-struct (range-reference expression) (range abs-x0? abs-y0? abs-x1? abs-y1?) #:transparent)

; (struct)
(define-struct (this-reference expression) () #:transparent)

; Quoting ----------------------------------------

; any -> boolean
(define (literal-value? item)
  (or (boolean? item)
      (integer? item)
      (real? item)
      (string? item)
      (symbol? item)))

; any -> boolean
(define (quotable? item)
  (or (formula? item)
      (expression? item)
      (range? item)
      (literal-value? item)))

; (U formula expression cell literal-value) -> expression
(define (quote-expression arg)
  (cond [(formula? arg)       (formula-expr arg)]
        [(expression? arg)    arg]
        [(range? arg)         (make-range-reference arg #f #f #f #f)]
        [(literal-value? arg) (make-literal arg)]))

; (U formula expression cell literal-value) -> formula
(define (quote-formula arg)
  (cond [(formula? arg)       arg]
        [(expression? arg)    (make-formula arg #f)]
        [(range? arg)         (make-formula (make-range-reference arg #f #f #f #f) #f)]
        [(literal-value? arg) (make-formula (make-literal arg) #f)]))

; Conditional formatting -------------------------

; (struct formula compiled-style natural)
(define-struct conditional-format (formula style priority) #:transparent)

; Validation -------------------------------------

; (struct formula (U 'error 'warning 'info) (U string #f) (U string #f) (U string #f) (U string #f))
(define-struct validation-rule (formula error-style error-title error-message prompt-title prompt-message) #:transparent)

; Provide statements -----------------------------

(provide/contract
 [struct data                         ()]
 [struct package-part                 ([id                            symbol?])]
 [struct (workbook package-part)      ([id                            symbol?]
                                       [sheets                        (listof worksheet?)])]
 [struct (worksheet package-part)     ([id                            symbol?]
                                       [name                          (string-length/c 31)]
                                       [data                          range?]
                                       [split                         (or/c split? #f)]
                                       [auto-filter                   (or/c auto-filter? #f)]
                                       [print-settings                (or/c print-settings? #f)]
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
 [struct split                        ([position                      (or/c range? (cons/c natural-number/c natural-number/c))]
                                       [scroll-position               (or/c range? (cons/c natural-number/c natural-number/c))]
                                       [frozen?                       boolean?])]
 [struct auto-filter                  ([x                             natural-number/c]
                                       [y                             natural-number/c]
                                       [width                         natural-number/c]
                                       [height                        natural-number/c])]
 [struct print-settings               ([fit-to-width                  (or/c (and/c integer? (>=/c 1)) #f)]
                                       [fit-to-height                 (or/c (and/c integer? (>=/c 1)) #f)]
                                       [orientation                   (or/c 'portrait 'landscape)]
                                       [headers                       (or/c string? #f)]
                                       [footers                       (or/c string? #f)])]
 [struct (range data)                 ([style                         style?]
                                       [validation-rule               (or/c validation-rule? #f)]
                                       [conditional-formats           (listof conditional-format?)])]
 [struct (cell range)                 ([style                         style?]
                                       [validation-rule               (or/c validation-rule? #f)]
                                       [conditional-formats           (listof conditional-format?)]
                                       [value                         quotable?]
                                       [dimensions                    (or/c cell-dims? #f)])]
 [struct cell-dims                    ([min-width                     (or/c (and/c number? (>=/c 0)) #f)]
                                       [max-width                     (or/c (and/c number? (>=/c 0)) #f)]
                                       [min-height                    (or/c (and/c number? (>=/c 0)) #f)]
                                       [max-height                    (or/c (and/c number? (>=/c 0)) #f)]
                                       [hide-row?                     boolean?]
                                       [hide-column?                  boolean?])]
 [struct (union range)                ([style                         style?]
                                       [validation-rule               (or/c validation-rule? #f)]
                                       [conditional-formats           (listof conditional-format?)]
                                       [parts                         (listof part?)]
                                       [width                         natural-number/c]
                                       [height                        natural-number/c])]
 [struct (part data)                  ([range                         range?]
                                       [dx                            natural-number/c]
                                       [dy                            natural-number/c])]
 [range-width                         (-> range? natural-number/c)]
 [range-height                        (-> range? natural-number/c)]
 [range-children                      (-> range? (listof range?))]
 [part-contains?                      (-> part? natural-number/c natural-number/c boolean?)]
 [struct formula                      ([expr expression?] [array? boolean?])]
 [struct expression                   ()]
 [struct (operator expression)        ([name symbol?] [args (listof expression?)])]
 [struct (function expression)        ([name symbol?] [args (listof expression?)])]
 [struct (array expression)           ([data (listof expression?)])]
 [struct (literal expression)         ([value literal-value?])]
 [struct (range-reference expression) ([range   range?]
                                       [abs-x0? boolean?]
                                       [abs-y0? boolean?]
                                       [abs-x1? boolean?]
                                       [abs-y1? boolean?])]
 [struct (this-reference expression)  ()]
 [literal-value?                      (-> any/c boolean?)]
 [quotable?                           (-> any/c boolean?)]
 [quote-expression                    (-> quotable? expression?)]
 [quote-formula                       (-> quotable? formula?)]
 [struct conditional-format           ([formula                       formula?]
                                       [style                         compiled-style?]
                                       [priority                      natural-number/c])]
 [struct validation-rule              ([formula                       formula?]
                                       [error-style                   (or/c 'stop 'warning 'information)]
                                       [error-title                   (or/c string? #f)]
                                       [error-message                 (or/c string? #f)]
                                       [prompt-title                  (or/c string? #f)]
                                       [prompt-message                (or/c string? #f)])])