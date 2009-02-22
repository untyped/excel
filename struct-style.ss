#lang scheme/base

(require "base.ss")

(require (unlib-in hash symbol)
         "ref.ss"
         (except-in "struct-style-internal.ss" empty-fill)) ; replaced by struct constant

; Colors -----------------------------------------

; [0,1] [0,1] [0,1] -> rgba-color
(define (make-rgb-color r g b)
  (make-rgba-color r g b 1))

; [0,1] [0,1] [0,1] -> rgba-color
(define rgb make-rgb-color)

; [0,1] [0,1] [0,1] [0,1] -> rgba-color
(define rgba make-rgba-color)

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

; font font -> font
(define (compose-fonts font1 font2)
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
             (compose-boolean font-subscript-raw   font1 font2)))

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

; Borders ----------------------------------------

;  [#:top            (U line #f)]
;  [#:right          (U line #f)]
;  [#:bottom         (U line #f)]
;  [#:left           (U line #f)]
;  [#:horizontal     (U line #f)]
;  [#:vertical       (U line #f)]
;  [#:diagonal       (U line #f)]
;  [#:outline?       (U boolean void)]
;  [#:diagonal-down? (U boolean void)]
;  [#:diagonal-up?   (U boolean void)]
; ->
;  border
(define (create-border #:top            [top               #f]
                       #:right          [right             #f]
                       #:bottom         [bottom            #f]
                       #:left           [left              #f]
                       #:horizontal     [horizontal        #f]
                       #:vertical       [vertical          #f]
                       #:diagonal       [diagonal          #f]
                       #:outline?       [outline-raw       (void)]
                       #:diagonal-down? [diagonal-down-raw (void)]
                       #:diagonal-up?   [diagonal-up-raw   (void)])
  (make-border top right bottom left
               horizontal vertical diagonal
               outline-raw
               (or (and diagonal
                        (void? diagonal-down-raw)
                        (void? diagonal-up-raw))
                   diagonal-down-raw)
               diagonal-up-raw))

; border -> boolean
(define (empty-border? border)
  (and (not   (border-top               border))
       (not   (border-right             border))
       (not   (border-bottom            border))
       (not   (border-left              border))
       (not   (border-horizontal        border))
       (not   (border-vertical          border))
       (not   (border-diagonal          border))
       (void? (border-outline-raw       border))
       (void? (border-diagonal-down-raw border))
       (void? (border-diagonal-up-raw   border))))

; border border -> border
(define (compose-borders border1 border2)
  (make-border (compose-line    border-top               border1 border2)
               (compose-line    border-right             border1 border2)
               (compose-line    border-bottom            border1 border2)
               (compose-line    border-left              border1 border2)
               (compose-line    border-horizontal        border1 border2)
               (compose-line    border-vertical          border1 border2)
               (compose-line    border-diagonal          border1 border2)
               (compose-boolean border-outline-raw       border1 border2)
               (compose-boolean border-diagonal-down-raw border1 border2)
               (compose-boolean border-diagonal-up-raw   border1 border2)))

; (border -> (U line #f)) border border -> (U line #f)
(define (compose-line accessor border1 border2)
  (let ([line1 (accessor border1)]
        [line2 (accessor border2)])
    (if line1
        (if line2
            (if (> (border-style-priority (line-style line1))
                   (border-style-priority (line-style line2)))
                line1
                line2)
            line1)
        line2)))

; border
(define empty-border (create-border))

; [border-style] [color] -> line
(define (create-line [style (border-style thin)] [color (make-rgba-color 0 0 0 1)])
  (make-line style color))

; Alignments -------------------------------------

;  [#:horizontal         (U horizontal-indent #f)]
;  [#:vertical           (U vertical-indent #f)]
;  [#:wrap?              (U boolean void)]
;  [#:shrink?            (U boolean void)]
;  [#:rotation           (U [0,359] #f)]
;  [#:reading-order      (U reading-order #f)]
;  [#:justify-last-line? (U boolean void)]
;  [#:indent             (U natural #f)]
;  [#:relative-indent    (U natural #f)]
; ->
;  alignment
(define (create-alignment #:horizontal         [horizontal            #f]
                          #:vertical           [vertical              #f]
                          #:wrap?              [wrap-raw              (void)]
                          #:shrink?            [shrink-raw            (void)]
                          #:rotation           [rotation              #f]
                          #:reading-order      [reading-order         #f]
                          #:justify-last-line? [justify-last-line-raw (void)]
                          #:indent             [indent                #f]
                          #:relative-indent    [relative-indent       #f])
  (make-alignment horizontal
                  vertical
                  wrap-raw
                  shrink-raw
                  rotation
                  reading-order
                  justify-last-line-raw
                  indent
                  relative-indent))

; alignment -> boolean
(define (alignment-wrap? align)              (eq? (alignment-wrap-raw align)              #t))
(define (alignment-shrink? align)            (eq? (alignment-shrink-raw align)            #t))
(define (alignment-justify-last-line? align) (eq? (alignment-justify-last-line-raw align) #t))

; alignment -> boolean
(define (empty-alignment? align)
  (and (not (alignment-horizontal align))
       (not (alignment-vertical align))
       (void? (alignment-wrap-raw align))
       (void? (alignment-shrink-raw align))
       (not (alignment-rotation align))
       (not (alignment-reading-order align))
       (void? (alignment-justify-last-line-raw align))
       (not (alignment-indent align))
       (not (alignment-relative-indent align))))

; alignment alignment -> alignment
(define (compose-alignments align1 align2)
  (make-alignment (compose-normal  alignment-horizontal            align1 align2)
                  (compose-normal  alignment-vertical              align1 align2)
                  (compose-boolean alignment-wrap-raw              align1 align2)
                  (compose-boolean alignment-shrink-raw            align1 align2)
                  (compose-normal  alignment-rotation              align1 align2)
                  (compose-normal  alignment-reading-order         align1 align2)
                  (compose-boolean alignment-justify-last-line-raw align1 align2)
                  (compose-normal  alignment-indent                align1 align2)
                  (compose-normal  alignment-relative-indent       align1 align2)))

; alignment
(define empty-alignment (create-alignment))

; Styles -----------------------------------------

;  [#:number-format number-format]
;  [#:font          font]
;  [#:fill          fill]
;  [#:border        border]
;  [#:alignment     alignment]
;  [#:hidden?       (U boolean void)]
;  [#:locked?       (U boolean void)]
; ->
;  style
(define (create-compiled-style
         #:number-format [fmt        empty-number-format]
         #:font          [font       empty-font]
         #:fill          [fill       empty-fill]
         #:border        [border     empty-border]
         #:alignment     [alignment  empty-alignment]
         #:hidden?       [hidden-raw (void)]
         #:locked?       [locked-raw (void)])
  (make-compiled-style fmt font fill border alignment hidden-raw locked-raw))

; style natural natural -> compiled-style
;
; Coordinates are supplied relative to the range to which the style is attached.
(define (compile-style style x y)
  (if (compiled-style? style)
      style
      ((uncompiled-style-proc style) x y)))

; style natural natural -> style
(define (translate-style style dx dy)
  (if (compiled-style? style)
      style
      (make-uncompiled-style
       (lambda (x y)
         (compile-style style (+ x dx) (+ y dy))))))

; compiled-style -> boolean
(define (compiled-style-hidden? style) (eq? (compiled-style-hidden-raw style) #t))
(define (compiled-style-locked? style) (eq? (compiled-style-locked-raw style) #t))

; style -> boolean
#;(define (empty-style? style)
    (and (compiled-style? style)
         (empty-number-format? (style-number-format style))
         (empty-font?          (style-font          style))
         (empty-fill?          (style-fill          style))
         (empty-border?        (style-border        style))
         (empty-alignment?     (style-alignment     style))
         (void? (style-hidden-raw style))
         (void? (style-locked-raw style))))

; style style -> style
(define (compose-styles style1 style2)
  (if (and (compiled-style? style1)
           (compiled-style? style2))
      (make-compiled-style
       (compose-number-formats (compiled-style-number-format style1)
                               (compiled-style-number-format style2))
       (compose-fonts          (compiled-style-font          style1)
                               (compiled-style-font          style2))
       (compose-fills          (compiled-style-fill          style1)
                               (compiled-style-fill          style2))
       (compose-borders        (compiled-style-border        style1)
                               (compiled-style-border        style2))
       (compose-alignments     (compiled-style-alignment     style1)
                               (compiled-style-alignment     style2))
       (compose-boolean compiled-style-hidden-raw style1 style2)
       (compose-boolean compiled-style-locked-raw style1 style2))
      (make-uncompiled-style
       (lambda (x y)
         (compose-styles (compile-style style1 x y)
                         (compile-style style2 x y))))))

; compiled-style
(define empty-style (create-compiled-style))

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
                     full-circle-degrees/c
                     half-circle-degrees/c
                     fraction/c
                     make-color            ; no direct cosntructor
                     make-number-format    ; optional argument constructor
                     make-font             ; keyword constructor
                     make-fill             ; no direct constructor
                     make-border           ; keyword constructor
                     make-line             ; optional argument constructor
                     make-alignment        ; keyword constructor
                     make-empty-fill       ; no direct constructor
                     make-style            ; no direct constructor
                     make-compiled-style)) ; keyword constructor

(provide/contract
 [make-rgb-color                                 (-> fraction/c fraction/c fraction/c rgba-color?)]
 [rgb                                            (-> fraction/c fraction/c fraction/c rgba-color?)]
 [rgba                                           (-> fraction/c fraction/c fraction/c fraction/c rgba-color?)]
 [rename create-number-format make-number-format   (->* () ((or/c string? #f)) number-format?)]
 [empty-number-format?                             (-> number-format? boolean?)]
 [compose-number-formats                           (-> number-format? number-format? number-format?)]
 [empty-number-format                              number-format?]
 [general-number-format                            number-format?]
 [rename create-font make-font                     (->* ()
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
 [font-bold?                                       (-> font? boolean?)]
 [font-italic?                                     (-> font? boolean?)]
 [font-underline?                                  (-> font? boolean?)]
 [font-outline?                                    (-> font? boolean?)]
 [font-shadow?                                     (-> font? boolean?)]
 [font-strike?                                     (-> font? boolean?)]
 [font-superscript?                                (-> font? boolean?)]
 [font-subscript?                                  (-> font? boolean?)]
 [empty-font?                                      (-> font? boolean?)]
 [compose-fonts                                    (-> font? font? font?)]
 [empty-font                                       font?]
 [make-solid-fill                                  (-> color? fill?)]
 [solid-fill?                                      (-> fill? boolean?)]
 [compose-fills                                    (-> fill? fill? fill?)]
 [empty-fill                                       fill?]
 [gray-125-fill                                    fill?]
 [rename create-border make-border                 (->* ()
                                                        (#:top (or/c line? #f)
                                                               #:right          (or/c line? #f)
                                                               #:bottom         (or/c line? #f)
                                                               #:left           (or/c line? #f)
                                                               #:horizontal     (or/c line? #f)
                                                               #:vertical       (or/c line? #f)
                                                               #:diagonal       (or/c line? #f)
                                                               #:outline?       (or/c boolean? void?)
                                                               #:diagonal-down? (or/c boolean? void?)
                                                               #:diagonal-up?   (or/c boolean? void?))
                                                        border?)]
 [empty-border?                                    (-> border? boolean?)]
 [compose-borders                                  (-> border? border? border?)]
 [empty-border                                     border?]
 [rename create-line make-line                     (->* () (border-style? color?) line?)]
 [rename create-alignment make-alignment           (->* ()
                                                        (#:horizontal horizontal-alignment?
                                                                      #:vertical           vertical-alignment?
                                                                      #:wrap?              (or/c boolean? void?)
                                                                      #:shrink?            (or/c boolean? void?)
                                                                      #:rotation           (or/c half-circle-degrees/c #f)
                                                                      #:reading-order      (or/c reading-order? #f)
                                                                      #:justify-last-line? (or/c boolean? void?)
                                                                      #:indent             (or/c natural-number/c #f)
                                                                      #:relative-indent    (or/c natural-number/c #f))
                                                        alignment?)]
 [alignment-wrap?                                  (-> alignment? boolean?)]
 [alignment-shrink?                                (-> alignment? boolean?)]
 [alignment-justify-last-line?                     (-> alignment? boolean?)]
 [empty-alignment?                                 (-> alignment? boolean?)]
 [compose-alignments                               (-> alignment? alignment? alignment?)]
 [empty-alignment                                  alignment?]
 [rename create-compiled-style make-compiled-style (->* ()
                                                        (#:number-format number-format?
                                                                         #:font      font?
                                                                         #:fill      fill?
                                                                         #:border    border?
                                                                         #:alignment alignment?
                                                                         #:hidden?   (or/c boolean? void?)
                                                                         #:locked?   (or/c boolean? void?))
                                                        style?)]
 [compile-style                                    (-> style? natural-number/c natural-number/c compiled-style?)]
 [translate-style                                  (-> style? natural-number/c natural-number/c style?)]
 [compiled-style-hidden?                           (-> compiled-style? boolean?)]
 [compiled-style-locked?                           (-> compiled-style? boolean?)]
 #;[empty-style?                                     (-> style? boolean?)]
 [compose-styles                                   (-> style? (or/c style? #f) style?)]
 [empty-style                                      compiled-style?])