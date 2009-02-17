#lang scheme/base

(require "base.ss")

(require (for-syntax scheme/base))

; Colors -----------------------------------------

; (struct)
(define-struct color () #:transparent)

; (struct [0,1] [0,1] [0,1] [0,1])
(define-struct (rgba-color color) (r g b a) #:transparent)

; rgba-color -> string
(define rgba-color-hex
  (match-lambda
    [(struct rgba-color (r g b a))
     (let* ([hex-digit  (match-lambda
                          [0  #\0] [1  #\1] [2  #\2] [3  #\3]
                          [4  #\4] [5  #\5] [6  #\6] [7  #\7]
                          [8  #\8] [9  #\9] [10 #\A] [11 #\B]
                          [12 #\C] [13 #\D] [14 #\E] [15 #\F])]
            [hex-digits (lambda (fraction)
                          (let ([int (inexact->exact (floor (* fraction 255)))])
                            (list (hex-digit (quotient int 16))
                                  (hex-digit (remainder int 16)))))])
       (apply string (append (hex-digits a)
                             (hex-digits r)
                             (hex-digits g)
                             (hex-digits b))))]))

; Number formats ---------------------------------

; (struct (U string #f))
; A code of #f means no formatting (will not override other formats when composing).
; A code of "" means "general" formatting (will override other formats when composing).
(define-struct number-format (code) #:transparent)

; Fonts ------------------------------------------

; (struct (U string #f)
;         (U natural #f)
;         (U color #f)
;         (U boolean void)
;         (U boolean void)
;         (U boolean void)
;         (U boolean void)
;         (U boolean void)
;         (U boolean void))
(define-struct font
  (name size color bold-raw italic-raw underline-raw outline-raw shadow-raw strike-raw superscript-raw subscript-raw)
  #:transparent)

; Fills ------------------------------------------

; (struct)
(define-struct fill () #:transparent)

; (struct)
(define-struct (empty-fill fill) () #:transparent)

; (struct natural (listof gradient-stop))
(define-struct (linear-gradient-fill fill) (angle stops) #:transparent)

; (struct [0,1] [0,1] [0,1] [0,1] (listof gradient-stop))
(define-struct (path-gradient-fill fill) (top right bottom left stops) #:transparent)

; (struct [0,1] color)
(define-struct gradient-stop (position color) #:transparent)

; (struct color color natural)
(define-struct (pattern-fill fill) (fg bg type) #:transparent)

; symbol
(define-syntax (pattern-type stx)
  (syntax-case stx ()
    [(_ val)
     (identifier? #'val)
     (case (syntax->datum #'val)
       [(empty)            #''none]
       [(dark-down)        #''darkDown]
       [(dark-gray)        #''darkGray]
       [(dark-grid)        #''darkGrid]
       [(dark-horizontal)  #''darkHorizontal]
       [(dark-trellis)     #''darkTrellis]
       [(dark-up)          #''darkUp]
       [(dark-vertical)    #''darkVertical]
       [(gray-0625)        #''gray0625]
       [(gray-125)         #''gray125]
       [(light-down)       #''lightDown]
       [(light-gray)       #''lightGray]
       [(light-grid)       #''lightGrid]
       [(light-horizontal) #''lightHorizontal]
       [(light-trellis)    #''lightTrellis]
       [(light-up)         #''lightUp]
       [(light-vertical)   #''lightVertical]
       [(medium-gray)      #''mediumGray]
       [(solid)            #''solid]
       [else               (raise-syntax-error #f "invalid pattern type" #'val stx)])]))

; any -> boolean
(define (pattern-type? val)
  (and (memq val '(none darkDown darkGray darkGrid darkHorizontal darkTrellis darkUp darkVertical
                        gray0625 gray125
                        lightDown lightGray lightGrid lightHorizontal lightTrellis lightUp lightVertical
                        mediumGray solid))
       #t))

; Styles -----------------------------------------

; (struct number-format
;         font
;         fill
;         (U boolean void)
;         (U boolean void))
(define-struct style (number-format font fill hidden-raw locked-raw) #:transparent)

; Provide statements -----------------------------

(define degrees/c
  (integer-in 0 359))

(define fraction/c
  (and/c number? (>=/c 0) (<=/c 1)))

(provide pattern-type)

(provide/contract
 [struct color                       ()]
 [struct (rgba-color color)          ([r               fraction/c]
                                      [g               fraction/c]
                                      [b               fraction/c]
                                      [a               fraction/c])]
 [rgba-color-hex                     (-> rgba-color? string?)]
 [struct number-format               ([code            (or/c string? #f)])]
 [struct font                        ([name            (or/c string? #f)]
                                      [size            (or/c string? #f)]
                                      [color           (or/c string? #f)]
                                      [bold-raw        (or/c boolean? void?)]
                                      [italic-raw      (or/c boolean? void?)]
                                      [underline-raw   (or/c boolean? void?)]
                                      [outline-raw     (or/c boolean? void?)]
                                      [shadow-raw      (or/c boolean? void?)]
                                      [strike-raw      (or/c boolean? void?)]
                                      [superscript-raw (or/c boolean? void?)]
                                      [subscript-raw   (or/c boolean? void?)])]
 [struct style                       ([number-format   number-format?]
                                      [font            font?]
                                      [fill            fill?]
                                      [hidden-raw      (or/c boolean? void?)]
                                      [locked-raw      (or/c boolean? void?)])]
 [struct fill                        ()]
 [struct (empty-fill fill)           ()]
 [struct (pattern-fill fill)         ([fg              color?]
                                      [bg              color?]
                                      [type            pattern-type?])]
 [struct (linear-gradient-fill fill) ([angle           degrees/c]
                                      [stops           (listof gradient-stop?)])]
 [struct (path-gradient-fill fill)   ([top             fraction/c] 
                                      [right           fraction/c]
                                      [bottom          fraction/c]
                                      [left            fraction/c]
                                      [stops           (listof gradient-stop?)])]
 [struct gradient-stop               ([position        fraction/c]
                                      [color           color?])]
 [pattern-type?                      (-> any/c boolean?)])
