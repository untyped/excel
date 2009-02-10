#lang scheme/base

(require (planet untyped/unlib:3/hash)
         (planet untyped/unlib:3/symbol)
         "base.ss"
         "ref.ss"
         "struct-style-internal.ss")

; Number formats ---------------------------------

; number-format
(define general-number-format
  (make-number-format #f))

; number-format -> boolean
(define (number-format-empty? fmt)
  (not (number-format-code fmt)))

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
(define (font-empty? font)
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
(define (font-compose font1 font2)
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

; Styles -----------------------------------------

;  #:number-format (U number-format #f)
;  #:font          (U font #f)
; ->
;  style
(define (create-style #:number-format [fmt  (make-number-format #f)]
                      #:font          [font (create-font)])
  (make-style fmt font))

; style -> boolean
(define (style-empty? style)
  (and (number-format-empty? (style-number-format style))
       (font-empty? (style-font style))))

; (U style #f) (U style #f) -> style
(define (style-compose style1 style2)
  (if style1
      (if style2
          (make-style (compose-normal style-number-format style1 style2)
                      (font-compose (style-font style1) (style-font style2)))
          style1)
      (if style2
          style2
          (create-style))))

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
                     make-font
                     make-style))

(provide/contract
 [general-number-format                  number-format?]
 [number-format-empty?                   (-> number-format? boolean?)]
 [rename create-font make-font           (->* ()
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
 [font-bold?                             (-> font? boolean?)]
 [font-italic?                           (-> font? boolean?)]
 [font-underline?                        (-> font? boolean?)]
 [font-outline?                          (-> font? boolean?)]
 [font-shadow?                           (-> font? boolean?)]
 [font-strike?                           (-> font? boolean?)]
 [font-superscript?                      (-> font? boolean?)]
 [font-subscript?                        (-> font? boolean?)]
 [font-empty?                            (-> font? boolean?)]
 [font-compose                           (-> (or/c font? #f) (or/c font? #f) font?)]
 [rename create-style make-style         (->* () (#:number-format (or/c number-format? #f)
                                                                  #:font (or/c font? #f)) style?)]
 [style-empty?                           (-> style? boolean?)]
 [style-compose                          (-> style? (or/c style? #f) style?)])