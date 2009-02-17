#lang scheme/base

(require "base.ss")

(require (unlib-in hash symbol)
         "ref.ss"
         (except-in "struct-style-internal.ss"
                    empty-fill)) ; struct type transformer binding replaced by struct constant

; Number formats ---------------------------------

; [(U string #f)] -> number-format
(define (create-number-format [code #f])
  (make-number-format code))

; number-format -> boolean
(define (empty-number-format? fmt)
  (not (number-format-code fmt)))

; number-format number-format -> number-format
(define (compose-number-formats fmt1 fmt2)
  (if (empty-number-format? fmt2)
      fmt1
      fmt2))

; number-format
(define empty-number-format   (create-number-format))
(define general-number-format (create-number-format ""))

; Fonts ------------------------------------------

;  #:name    (U string #f)
;  #:size    (U natural #f)
;  #:color   (U color #f)
;  #:bold?   (U bolean void)
;  #:italic? ...
; ->
; font
(define (create-font #:name         [name            #f]
                     #:size         [size            #f]
                     #:color        [color           #f]
                     #:bold?        [bold-raw        (void)]
                     #:italic?      [italic-raw      (void)]
                     #:underline?   [underline-raw   (void)]
                     #:outline?     [outline-raw     (void)]
                     #:shadow?      [shadow-raw      (void)]
                     #:strike?      [strike-raw      (void)]
                     #:superscript? [superscript-raw (void)]
                     #:subscript?   [subscript-raw   (void)])
  (make-font name 
             size
             color
             bold-raw
             italic-raw
             underline-raw
             outline-raw
             shadow-raw
             strike-raw
             superscript-raw
             subscript-raw))

; font -> boolean
(define (font-bold? font)        (eq? (font-bold-raw font)        #t))
(define (font-italic? font)      (eq? (font-italic-raw font)      #t))
(define (font-underline? font)   (eq? (font-underline-raw font)   #t))
(define (font-outline? font)     (eq? (font-outline-raw font)     #t))
(define (font-shadow? font)      (eq? (font-shadow-raw font)      #t))
(define (font-strike? font)      (eq? (font-strike-raw font)      #t))
(define (font-superscript? font) (eq? (font-superscript-raw font) #t))
(define (font-subscript? font)   (eq? (font-subscript-raw font)   #t))

; font -> boolean
(define (empty-font? font)
  (and (not (font-name font))
       (not (font-size font))
       (not (font-color font))
       (void? (font-bold-raw font))
       (void? (font-italic-raw font))
       (void? (font-underline-raw font))
       (void? (font-outline-raw font))
       (void? (font-shadow-raw font))
       (void? (font-strike-raw font))
       (void? (font-superscript-raw font))
       (void? (font-subscript-raw font))))

; (U font #f) (U font #f) -> font
(define (compose-fonts font1 font2)
  (if font1
      (if font2
          (make-font (compose-normal  font-name            font1 font2)
                     (compose-normal  font-size            font1 font2)
                     (compose-normal  font-color           font1 font2)
                     (compose-boolean font-bold-raw        font1 font2)
                     (compose-boolean font-italic-raw      font1 font2)
                     (compose-boolean font-underline-raw   font1 font2)
                     (compose-boolean font-outline-raw     font1 font2)
                     (compose-boolean font-shadow-raw      font1 font2)
                     (compose-boolean font-strike-raw      font1 font2)
                     (compose-boolean font-superscript-raw font1 font2)
                     (compose-boolean font-subscript-raw   font1 font2))
          font1)
      (if font2
          font2
          (create-font))))

; font
(define empty-font (create-font))

; Fills ------------------------------------------

; color -> fill
(define (make-solid-fill color)
  (make-pattern-fill color color (pattern-type solid)))

; fill -> boolean
(define (solid-fill? fill)
  (and (pattern-fill? fill)
       (eq? (pattern-fill-type fill) (pattern-type solid))
       #t))

(define fill-empty? empty-fill?)

; fill fill -> fill
(define (compose-fills fill1 fill2)
  (if (fill-empty? fill2)
      fill1
      fill2))

; fill
(define empty-fill (make-empty-fill))
(define gray-125-fill
  (make-pattern-fill (make-rgba-color 0 0 0 1)
                     (make-rgba-color 1 1 1 1)
                     (pattern-type gray-125)))

; Styles -----------------------------------------

;  [#:number-format number-format]
;  [#:font          font]
;  [#:fill          fill]
;  [#:hidden?       (U boolean void)]
;  [#:locked?       (U boolean void)]
; ->
;  style
(define (create-style #:number-format [fmt        empty-number-format]
                      #:font          [font       empty-font]
                      #:fill          [fill       empty-fill]
                      #:hidden?       [hidden-raw (void)]
                      #:locked?       [locked-raw (void)])
  (make-style fmt font fill hidden-raw locked-raw))

; style -> boolean
(define (style-hidden? style) (eq? (style-hidden-raw style) #t))
(define (style-locked? style) (eq? (style-locked-raw style) #t))

; style -> boolean
(define (empty-style? style)
  (and (empty-number-format? (style-number-format style))
       (empty-font? (style-font style))
       (void? (style-hidden-raw style))
       (void? (style-locked-raw style))))

; (U style #f) (U style #f) -> style
(define (compose-styles style1 style2)
  (if style1
      (if style2
          (make-style (compose-number-formats (style-number-format style1)
                                             (style-number-format style2))
                      (compose-fonts (style-font style1)
                                    (style-font style2))
                      (compose-fills (style-fill style1)
                                     (style-fill style2))
                      (compose-boolean style-hidden-raw style1 style2)
                      (compose-boolean style-locked-raw style1 style2))
          style1)
      (if style2
          style2
          (create-style))))

; style
(define empty-style (create-style))

; Helpers ----------------------------------------

; (any -> any) any any -> any
(define (compose-normal accessor struct1 struct2)
  (or (accessor struct2)
      (accessor struct1)))

; (any -> (U boolean void)) any any -> (U boolean void)
(define (compose-boolean accessor struct1 struct2)
  (let ([val1 (accessor struct1)]
        [val2 (accessor struct2)])
    (cond [(eq? val2 #t) #t]
          [(eq? val2 #f) #f]
          [(eq? val1 #t) #t]
          [(eq? val1 #f) #f]
          [else (void)])))

; Provide statements -----------------------------

(provide (except-out (all-from-out "struct-style-internal.ss")
                     make-color ; no direct cosntructor
                     make-number-format
                     make-font
                     make-fill ; no direct constructor
                     make-style))

(provide/contract
 [rename create-number-format make-number-format (->* () ((or/c string? #f)) number-format?)]
 [empty-number-format?                           (-> number-format? boolean?)]
 [compose-number-formats                         (-> number-format? number-format? number-format?)]
 [empty-number-format                            number-format?]
 [general-number-format                          number-format?]
 [rename create-font make-font                   (->* ()
                                                      (#:name (or/c string? #f)
                                                              #:size         (or/c natural-number/c #f)
                                                              #:color        (or/c color? #f)
                                                              #:bold?        (or/c boolean? void?)
                                                              #:italic?      (or/c boolean? void?)
                                                              #:underline?   (or/c boolean? void?)
                                                              #:outline?     (or/c boolean? void?)
                                                              #:shadow?      (or/c boolean? void?)
                                                              #:strike?      (or/c boolean? void?)
                                                              #:superscript? (or/c boolean? void?)
                                                              #:subscript?   (or/c boolean? void?))
                                                      font?)]
 [font-bold?                                     (-> font? boolean?)]
 [font-italic?                                   (-> font? boolean?)]
 [font-underline?                                (-> font? boolean?)]
 [font-outline?                                  (-> font? boolean?)]
 [font-shadow?                                   (-> font? boolean?)]
 [font-strike?                                   (-> font? boolean?)]
 [font-superscript?                              (-> font? boolean?)]
 [font-subscript?                                (-> font? boolean?)]
 [empty-font?                                    (-> font? boolean?)]
 [compose-fonts                                  (-> font? font? font?)]
 [empty-font                                     font?]
 [make-solid-fill                                (-> color? fill?)]
 [solid-fill?                                    (-> fill? boolean?)]
 [compose-fills                                  (-> fill? fill? fill?)]
 [empty-fill                                     fill?]
 [gray-125-fill                                  fill?]
 [rename create-style make-style                 (->* ()
                                                      (#:number-format number-format?
                                                                       #:font    font?
                                                                       #:fill    fill?
                                                                       #:hidden? (or/c boolean? void?)
                                                                       #:locked? (or/c boolean? void?))
                                                      style?)]
 [style-hidden?                                  (-> style? boolean?)]
 [style-locked?                                  (-> style? boolean?)]
 [empty-style?                                   (-> style? boolean?)]
 [compose-styles                                 (-> style? (or/c style? #f) style?)]
 [empty-style                                    style?])