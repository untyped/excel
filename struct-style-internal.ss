#lang scheme/base

(require "base.ss")

(require (for-syntax scheme/base
                     scheme/match))

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

; (_ id) -> symbol
(define-syntax (pattern-type stx)
  (syntax-case stx ()
    [(_ val)
     (identifier? #'val)
     (match (syntax->datum #'val)
       ['empty            #''none]
       ['dark-down        #''darkDown]
       ['dark-gray        #''darkGray]
       ['dark-grid        #''darkGrid]
       ['dark-horizontal  #''darkHorizontal]
       ['dark-trellis     #''darkTrellis]
       ['dark-up          #''darkUp]
       ['dark-vertical    #''darkVertical]
       ['gray-0625        #''gray0625]
       ['gray-125         #''gray125]
       ['light-down       #''lightDown]
       ['light-gray       #''lightGray]
       ['light-grid       #''lightGrid]
       ['light-horizontal #''lightHorizontal]
       ['light-trellis    #''lightTrellis]
       ['light-up         #''lightUp]
       ['light-vertical   #''lightVertical]
       ['medium-gray      #''mediumGray]
       ['solid            #''solid]
       [_                 (raise-syntax-error #f "invalid pattern type" #'val stx)])]))

; any -> boolean
(define (pattern-type? val)
  (and (memq val '(none darkDown darkGray darkGrid darkHorizontal darkTrellis darkUp darkVertical
                        gray0625 gray125
                        lightDown lightGray lightGrid lightHorizontal lightTrellis lightUp lightVertical
                        mediumGray solid))
       #t))

; Borders ----------------------------------------

; (struct (U line #f)
;         (U line #f)
;         (U line #f)
;         (U line #f)
;         (U line #f)
;         (U line #f)
;         (U line #f)
;         (U boolean void)
;         (U boolean void)
;         (U boolean void))
(define-struct border
  (top right bottom left horizontal vertical diagonal outline-raw diagonal-down-raw diagonal-up-raw)
  #:transparent)

; border -> boolean
(define (border-outline?       border) (eq? (border-outline-raw       border) #t))
(define (border-diagonal-down? border) (eq? (border-diagonal-down-raw border) #t))
(define (border-diagonal-up?   border) (eq? (border-diagonal-up-raw   border) #t))

; (struct border-style color)
(define-struct line (style color) #:transparent)

; (_ id) -> symbol
(define-syntax (border-style stx)
  (syntax-case stx ()
    [(_ val)
     (match (syntax->datum #'val)
       ['none                #''none]
       ['hair                #''hair]
       ['thin                #''thin]
       ['medium              #''medium]
       ['thick               #''thick]
       ['double              #''double]
       ['dotted              #''dotted]
       ['dashed              #''dashed]
       ['dash-dot            #''dashDot]
       ['dash-dot-dot        #''dashDotDot]
       ['medium-dashed       #''mediumDashed]
       ['medium-dash-dot     #''mediumDashDot]
       ['medium-dash-dot-dot #''mediumDashDotDot]
       ['slant-dash-dot      #''slantDashDot]
       [_                    (raise-syntax-error #f "invalid border style" #'val stx)])]))

; (listof symbol)
(define border-styles
  '(none hair thin medium thick double dotted dashed dashDot dashDotDot
         medium mediumDashed mediumDashDot mediumDashDotDot slantDashDot))

; border-style -> natural
;
; Used to determine priority when composing lines.
(define border-style-priority
  (match-lambda
    ['none              0]
    ['hair             10]
    ['dotted           16]
    ['dashDotDot       17]
    ['dashDot          18]
    ['dashed           19]
    ['thin             20]
    ['mediumDashDotDot 26]
    ['mediumDashDot    27]
    ['mediumDashed     28]
    ['slantDashDot     29]
    ['medium           30]
    ['thick            40]
    ['double           50]))

; any -> boolean
(define (border-style? val)
  (and (memq val border-styles) #t))

; Alignment --------------------------------------

; (struct (U horizontal-alignment #f)
;         (U vertical-alignment #f)
;         (U boolean void)
;         (U boolean void)
;         (U [0,359] #f)
;         (U reading-order #f)
;         (U boolean void)
;         (U natural void)
;         (U natural void))
(define-struct alignment
  (horizontal vertical wrap-raw shrink-raw rotation reading-order justify-last-line-raw indent relative-indent)
  #:transparent)

; (_ id) -> symbol
(define-syntax (horizontal-alignment stx)
  (syntax-case stx ()
    [(_ val)
     (identifier? #'val)
     (match (syntax->datum #'val)
       ['general           #''general]
       ['left              #''left]
       ['right             #''right]
       ['center            #''center]
       ['center-continuous #''centerContinuous]
       ['distributed       #''distributed]
       ['justify           #''justify]
       ['fill              #''fill]
       [_                  (raise-syntax-error #f "invalid horizontal alignment" stx #'val)])]))

; (listof symbol)
(define horizontal-alignments
  '(general left right center centerContinuous distributed justify fill))

; any -> boolean
(define (horizontal-alignment? val)
  (and (memq val horizontal-alignments) #t))

; (_ id) -> symbol
(define-syntax (vertical-alignment stx)
  (syntax-case stx ()
    [(_ val)
     (identifier? #'val)
     (match (syntax->datum #'val)
       ['top               #''top]
       ['bottom            #''bottom]
       ['center            #''center]
       ['distributed       #''distributed]
       ['justify           #''justify]
       [_                  (raise-syntax-error #f "invalid vertical alignment" stx #'val)])]))

; (listof symbol)
(define vertical-alignments
  '(top bottom center distributed justify))

; any -> boolean
(define (vertical-alignment? val)
  (and (memq val vertical-alignments) #t))

; (_ id) -> symbol
(define-syntax (reading-order stx)
  (syntax-case stx ()
    [(_ val)
     (identifier? #'val)
     (match (syntax->datum #'val)
       ['context-dependent #''context-dependent]
       ['left-to-right     #''left-to-right]
       ['right-to-left     #''right-to-left]
       [_                  (raise-syntax-error #f "invalid reading order" stx #'val)])]))

; (listof symbol)
(define reading-orders
  '(context-dependent left-to-right right-to-left))

; any -> boolean
(define (reading-order? val)
  (and (memq val reading-orders) #t))

; reading-order -> natural
(define reading-order-code
  (match-lambda
    ['context-dependent 0]
    ['left-to-right     1]
    ['right-to-left     2]))

; Styles -----------------------------------------

; (struct)
(define-struct style () #:transparent)

; (struct number-format
;         font
;         fill
;         alignment
;         (U boolean void)
;         (U boolean void))
(define-struct (compiled-style style)
  (number-format font fill border alignment hidden-raw locked-raw)
  #:transparent)

; (struct (natural natural -> static-style))
(define-struct (uncompiled-style style)
  (proc)
  #:transparent)

; Conditional formatting -------------------------

; (struct condition-type formula compiled-style natural)
(define-struct conditional-format (type formula style priority) #:transparent)

; (_ id) -> symbol
(define-syntax (condition-type stx)
  (syntax-case stx ()
    [(_ val)
     (identifier? #'val)
     (match (syntax->datum #'val)
       ['above-average       #''aboveAverage]
       ['begins-with         #''beginsWith]
       ['cell-is             #''cellIs]
       ['color-scale         #''colorScale]
       ['contains-blanks     #''containsBlanks]
       ['contains-errors     #''containsErrors]
       ['contains-text       #''containsText]
       ['data-bar            #''dataBar]
       ['duplicate-values    #''duplicateValues]
       ['ends-with           #''endsWith]
       ['expression          #''expression]
       ['icon-set            #''iconSet]
       ['not-contains-blanks #''notContainsBlanks]
       ['not-contains-errors #''notContainsErrors]
       ['not-contains-text   #''notContainsText]
       ['time-period         #''timePeriod]
       ['top10               #''top10]
       ['unique-values       #''uniqueValues]
       [_                  (raise-syntax-error #f "invalid conditional format type" stx #'val)])]))

; any -> boolean
(define (condition-type? type)
  (and (memq type '(aboveAverage beginsWith cellIs colorScale containsBlanks containsErrors containsText dataBar
                                 duplicateValues endsWith expression iconSet notContainsBlanks notContainsErrors notContainsText
                                 timePeriod top10 uniqueValues)) #t))

; Provide statements -----------------------------

(define full-circle-degrees/c
  (integer-in 0 359))

(define half-circle-degrees/c
  (integer-in 0 359))

(define fraction/c
  (and/c number? (>=/c 0) (<=/c 1)))

(provide full-circle-degrees/c
         half-circle-degrees/c
         fraction/c
         pattern-type
         horizontal-alignment
         vertical-alignment
         reading-order
         border-style
         condition-type)

(provide/contract
 [struct color                       ()]
 [struct (rgba-color color)          ([r                     fraction/c]
                                      [g                     fraction/c]
                                      [b                     fraction/c]
                                      [a                     fraction/c])]
 [rgba-color-hex                     (-> rgba-color? string?)]
 [struct number-format               ([code                  (or/c string? #f)])]
 [struct font                        ([name                  (or/c string? #f)]
                                      [size                  (or/c natural-number/c #f)]
                                      [color                 (or/c color? #f)]
                                      [bold-raw              (or/c boolean? void?)]
                                      [italic-raw            (or/c boolean? void?)]
                                      [underline-raw         (or/c boolean? void?)]
                                      [outline-raw           (or/c boolean? void?)]
                                      [shadow-raw            (or/c boolean? void?)]
                                      [strike-raw            (or/c boolean? void?)]
                                      [superscript-raw       (or/c boolean? void?)]
                                      [subscript-raw         (or/c boolean? void?)])]
 [struct fill                        ()]
 [struct (empty-fill fill)           ()]
 [struct (pattern-fill fill)         ([fg                    color?]
                                      [bg                    color?]
                                      [type                  pattern-type?])]
 [struct (linear-gradient-fill fill) ([angle                 full-circle-degrees/c]
                                      [stops                 (listof gradient-stop?)])]
 [struct (path-gradient-fill fill)   ([top                   fraction/c] 
                                      [right                 fraction/c]
                                      [bottom                fraction/c]
                                      [left                  fraction/c]
                                      [stops                 (listof gradient-stop?)])]
 [struct gradient-stop               ([position              fraction/c]
                                      [color                 color?])]
 [pattern-type?                      (-> any/c boolean?)]
 [struct border                      ([top               (or/c line? #f)]
                                      [right             (or/c line? #f)]
                                      [bottom            (or/c line? #f)]
                                      [left              (or/c line? #f)]
                                      [horizontal        (or/c line? #f)]
                                      [vertical          (or/c line? #f)]
                                      [diagonal          (or/c line? #f)]
                                      [outline-raw       (or/c boolean? void?)]
                                      [diagonal-down-raw (or/c boolean? void?)]
                                      [diagonal-up-raw   (or/c boolean? void?)])]
 [border-outline?                    (-> border? boolean?)]
 [border-diagonal-down?              (-> border? boolean?)]
 [border-diagonal-up?                (-> border? boolean?)]
 [struct line                        ([style border-style?]
                                      [color color?])]
 [border-styles                      (listof symbol?)]
 [border-style?                      (-> any/c boolean?)]
 [border-style-priority              (-> border-style? natural-number/c)]
 [struct alignment                   ([horizontal            (or/c horizontal-alignment? #f)]
                                      [vertical              (or/c vertical-alignment? #f)]
                                      [wrap-raw              (or/c boolean? void?)]
                                      [shrink-raw            (or/c boolean? void?)]
                                      [rotation              (or/c half-circle-degrees/c #f)]
                                      [reading-order         (or/c reading-order? #f)]
                                      [justify-last-line-raw (or/c boolean? void?)]
                                      [indent                (or/c natural-number/c #f)]
                                      [relative-indent       (or/c natural-number/c #f)])]
 [horizontal-alignments              (listof symbol?)]
 [horizontal-alignment?              (-> any/c boolean?)]
 [vertical-alignments                (listof symbol?)]
 [vertical-alignment?                (-> any/c boolean?)]
 [reading-orders                     (listof symbol?)]
 [reading-order?                     (-> any/c boolean?)]
 [reading-order-code                 (-> reading-order? natural-number/c)]
 [struct style                       ()]
 [struct (compiled-style style)      ([number-format         number-format?]
                                      [font                  font?]
                                      [fill                  fill?]
                                      [border                border?]
                                      [alignment             alignment?]
                                      [hidden-raw            (or/c boolean? void?)]
                                      [locked-raw            (or/c boolean? void?)])]
 [struct (uncompiled-style style)    ([proc                  (-> natural-number/c natural-number/c compiled-style?)])]
 [struct conditional-format          ([type                  condition-type?]
                                      [formula               any/c] ; formula?
                                      [style                 compiled-style?]
                                      [priority              natural-number/c])]
 [condition-type?                    (-> any/c boolean?)])
