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
;  [#:split             (U split #f)]
;  [#:auto-filter       (U auto-filter #f)]
;  [#:print-settings    (U print-settings #f)]
;  [#:protect?          boolean]
;  [#:auto-filter-lock? boolean]
;  ...
; ->
;  worksheet
(define (create-worksheet #:id [id (gensym/interned 'sheet)]
                          name
                          data
                          #:split                         [split                         #f]
                          #:auto-filter                   [auto-filter                   #f]
                          #:print-settings                [print-settings                #f]
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
                  sort-lock?))

;  [#:fit-to-width  (U natural>=1 #f)]
;  [#:fit-to-height (U natural>=1 #f)]
;  [#:orientation   (U 'portrait 'landscape)]
;  [#:headers       (U string #f)]
;  [#:footers       (U string #f)]
; ->
;  print-settings
(define (create-print-settings
         #:fit-to-width  [fit-to-width  #f]
         #:fit-to-height [fit-to-height #f]
         #:orientation   [orientation   'portrait]
         #:headers       [headers       #f]
         #:footers       [footers       #f])
  (make-print-settings
   fit-to-width
   fit-to-height
   orientation
   headers
   footers))

; Range wrappers ---------------------------------

; (listof part) natural natural [style] [#:validate (U validation-rule #f)] [#:cf (listof conditional-format)] -> union
(define (create-union parts x y [style empty-style] #:validate [validation-rule #f] #:cf [conditional-formats null])
  (make-union style validation-rule conditional-formats parts x y))

;  any
;  [style]
;  [#:validate     (U validation-rule #f)]
;  [#:cf           (listof conditional-format)]
;  [#:min-width    (U 0+ #f)]
;  [#:max-width    (U 0+ #f)]
;  [#:min-height   (U 0+ #f)]
;  [#:max-height   (U 0+ #f)]
;  [#:hide-row?    boolean]
;  [#:hide-column? boolean]
; ->
;  cell
(define (create-cell val            [style               empty-style]
                     #:validate     [validation-rule     #f]
                     #:cf           [conditional-formats null]
                     #:min-width    [min-width           #f]
                     #:max-width    [max-width           #f]
                     #:min-height   [min-height          #f]
                     #:max-height   [max-height          #f]
                     #:hide-row?    [hide-row?           #f]
                     #:hide-column? [hide-column?        #f])
  (make-cell style
             validation-rule
             conditional-formats
             val
             (and (or min-width max-width min-height max-height hide-row? hide-column?)
                  (make-cell-dims min-width 
                                  max-width
                                  min-height
                                  max-height
                                  hide-row?
                                  hide-column?))))

; Formula and expression wrappers ----------------

; (U expression quotable) [boolean] -> formula
(define (create-formula expr [array? #f])
  (make-formula (quote-expression expr) array?))

; symbol (U expression quotable) ... -> operator
(define (create-operator name . args)
  (make-operator name (map quote-expression args)))

; symbol (U expression quotable) ... -> function
(define (create-function name . args)
  (let ([max-arity (max-function-arity)])
    (when (and max-arity (> (length args) max-arity))
      (error (format "too many arguments (~a out of ~a)"
                     (length args)
                     max-arity))))
  (let ([max-depth (max-function-nesting-depth)]
        [ans (make-function name (map quote-expression args))])
    (when (and max-depth (> (expression-function-nesting-depth ans) max-depth))
      (error (format "too many levels of function nesting (~a out of ~a)" 
                     (expression-function-nesting-depth ans)
                     max-depth)))
    ans))

; symbol (U expression quotable) ... -> expression
(define (optimize-commutative-function name . args)
  (let ([max-arity (max-function-arity)]
        [arity     (length args)])
    (if (and max-arity (> arity max-arity))
        (let loop ([args args] [arg-accum null] [group-accum null])
          (match args
            [(list)
             (if (= (length arg-accum) max-arity)
                 (apply optimize-commutative-function name (append (reverse group-accum) (list (apply create-function name (reverse arg-accum)))))
                 (apply optimize-commutative-function name (append (reverse group-accum) (reverse arg-accum))))]
            [(list-rest curr rest)
             (if (= (length arg-accum) max-arity)
                 (loop rest
                       (list curr)
                       (cons (apply create-function name (reverse arg-accum)) group-accum))
                 (loop rest
                       (cons curr arg-accum)
                       group-accum))]))
        (apply create-function name args))))

; (U expression quotable) ... -> array
(define (create-array . args)
  (make-array (map quote-expression args)))

; literal-value -> literal
(define (create-literal val)
  (cond [(boolean? val) (make-literal val)]
        [(integer? val) (make-literal val)]
        [(real? val)    (make-literal val)]
        [(string? val)  (make-literal val)]
        [(symbol? val)  (make-literal val)]
        [else           (raise-exn exn:fail:contract
                          (format "Expected (U boolean integer real string symbol), received ~s" val))]))

; cell [boolean] [boolean] [boolean] [boolean] -> range-reference
(define (create-range-reference cell [abs-x0? #f] [abs-y0? #f] [abs-x1? abs-x0?] [abs-y1? abs-y0?])
  (make-range-reference cell abs-x0? abs-y0? abs-x1? abs-y1?))

; expression -> natural
(define expression-function-nesting-depth
  (match-lambda
    [(struct function (_ args))
     (add1 (apply max (cons 0 (map expression-function-nesting-depth args))))]
    [(struct operator (_ args))
     (apply max (cons 0 (map expression-function-nesting-depth args)))]
    [(struct array (data))
     (apply max (cons 0 (map expression-function-nesting-depth data)))]
    [(struct formula (expr _))
     (expression-function-nesting-depth expr)]
    [_ 0]))

; Validation rules -------------------------------

;  quotable
;  [#:error-style    (U 'error 'warning 'info)]
;  [#:error-title    (U string #f)]
;  [#:error-message  (U string #f)]
;  [#:prompt-title   (U string #f)]
;  [#:prompt-message (U string #f)]
; ->
;  validation-rule
(define (validate
         fx
         #:error-style    [error-style    'stop]
         #:error-title    [error-title    #f]
         #:error-message  [error-message  #f]
         #:prompt-title   [prompt-title   #f]
         #:prompt-message [prompt-message #f])
  (make-validation-rule (quote-formula fx)
                        error-style
                        error-title
                        error-message
                        prompt-title
                        prompt-message))

; Provide statements -----------------------------

(provide (except-out (all-from-out "struct-internal.ss")
                     make-workbook
                     make-worksheet
                     make-print-settings
                     make-union
                     make-cell
                     make-formula
                     make-operator
                     make-function
                     make-array
                     make-range-reference
                     make-literal)
         (all-from-out "struct-style.ss"))

(provide/contract
 [rename create-workbook       make-workbook         (->* () (#:id symbol? (listof worksheet?)) workbook?)]
 [rename create-worksheet      make-worksheet        (->* (string? range?)
                                                          (#:id symbol?
                                                                #:split                         (or/c split? #f)
                                                                #:auto-filter                   (or/c auto-filter? #f)
                                                                #:print-settings                (or/c print-settings? #f)
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
 [rename create-print-settings make-print-settings   (->* ()
                                                          (#:fit-to-width (or/c (and/c integer? (>=/c 1)) #f)
                                                                          #:fit-to-height (or/c (and/c integer? (>=/c 1)) #f)
                                                                          #:orientation   (or/c 'portrait 'landscape)
                                                                          #:headers       (or/c string? #f)
                                                                          #:footers       (or/c string? #f))
                                                          print-settings?)]
 [rename create-union          make-union            (->* ((listof part?) natural-number/c natural-number/c)
                                                          (style? #:validate (or/c validation-rule? #f) #:cf (listof conditional-format?))
                                                          union?)]
 [rename create-cell           make-cell             (->* (quotable?)
                                                          (style? #:validate     (or/c validation-rule? #f)
                                                                  #:cf           (listof conditional-format?)
                                                                  #:min-width    (or/c (and/c number? (>=/c 0)) #f)
                                                                  #:max-width    (or/c (and/c number? (>=/c 0)) #f)
                                                                  #:min-height   (or/c (and/c number? (>=/c 0)) #f)
                                                                  #:max-height   (or/c (and/c number? (>=/c 0)) #f)
                                                                  #:hide-row?    boolean?
                                                                  #:hide-column? boolean?)
                                                          cell?)]
 [rename create-formula        make-formula          (->* (quotable?) (boolean?) formula?)]
 [rename create-operator       make-operator         (->* (symbol?) () #:rest (listof quotable?) operator?)]
 [rename create-function       make-function         (->* (symbol?) () #:rest (listof quotable?) function?)]
 [optimize-commutative-function                      (->* (symbol?) () #:rest (listof quotable?) function?)]
 [rename create-array          make-array            (->* () () #:rest (listof quotable?) array?)]
 [rename create-literal        make-literal          (-> literal-value? literal?)]
 [rename create-range-reference make-range-reference (->* (range?) (boolean? boolean? boolean? boolean?) range-reference?)]
 [expression-function-nesting-depth                  (-> expression? natural-number/c)]
 [validate                                           (->* (quotable?)
                                                          (#:error-style (or/c 'stop 'warning 'information)
                                                                         #:error-title    (or/c string? #f)
                                                                         #:error-message  (or/c string? #f)
                                                                         #:prompt-title   (or/c string? #f)
                                                                         #:prompt-message (or/c string? #f))
                                                          validation-rule?)])