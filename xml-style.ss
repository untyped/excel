#lang scheme/base

(require "base.ss")

(require "struct.ss"
         "xml-cache.ss"
         "xml-internal.ss")

; Procedures -------------------------------------

; cache workbook -> xml
(define (styles-xml! cache book)
  
  ; (hashof (U number-format font etc...) natural)
  (define style-hash (make-hash))
  
  ; (box (listof xml))
  (define fmt-accum    (box null))
  (define font-accum   (box null))
  (define fill-accum   (box null))
  (define border-accum (box null))
  (define style-accum  (box null))
  
  ; number-format  -> natural
  ; font           -> natural
  ; etc...
  (define consume-number-format! (make-number-format-consumer style-hash fmt-accum))
  (define consume-font!          (make-font-consumer          style-hash font-accum))
  (define consume-fill!          (make-fill-consumer          style-hash fill-accum))
  (define consume-border!        (make-border-consumer        style-hash border-accum))
  (define consume-style!         (make-style-consumer         style-hash style-accum))
  
  ; worksheet natural natural (U cell #f) style -> void
  (define (accumulate! sheet x y cell style)
    (let ([style-id (consume-style!
                     style
                     (consume-number-format! (compiled-style-number-format style))
                     (consume-font!          (compiled-style-font          style))
                     (consume-fill!          (compiled-style-fill          style))
                     (consume-border!        (compiled-style-border        style)))])
      (cache-forward-set! cache sheet x y cell style-id)
      (when cell (cache-reverse-set! cache cell sheet x y))))
  
  ; void
  (for ([sheet (in-list (workbook-sheets book))])
    (range-fold (worksheet-data sheet) empty-style (cut accumulate! sheet <> <> <> <>)))
  
  ; xml
  (xml (numFmts (@ [count ,(length (unbox fmt-accum))])
                ,@(reverse (unbox fmt-accum)))
       (fonts   (@ [count ,(length (unbox font-accum))])
                ,@(reverse (unbox font-accum)))
       (fills   (@ [count ,(length (unbox fill-accum))])
                ,@(reverse (unbox fill-accum)))
       (borders (@ [count ,(length (unbox border-accum))])
                ,@(reverse (unbox border-accum)))
       (cellXfs (@ [count ,(length (unbox style-accum))])
                ,@(reverse (unbox style-accum)))))

; Style element consumers ------------------------

; (hashof number-format natural) (box (listof number-format)) -> (number-format -> natural)
(define (make-number-format-consumer hash accum)
  ; -> natural
  (define next-id (make-counter 100))
  
  ; number-format -> natural
  (define (consume! fmt)
    (or (hash-ref hash fmt #f)
        (let ([id (next-id)])
          (hash-set! hash fmt id)
          (set-box! accum (cons (number-format-xml fmt id) (unbox accum)))
          id)))
  
  (hash-set! hash empty-number-format 0)
  consume!)

; (hashof font natural) (box (listof font)) -> (font -> natural)
(define (make-font-consumer hash accum)
  ; -> natural
  (define next-id (make-counter 0))
  
  ; font -> natural
  (define (consume! font)
    (or (hash-ref hash font #f)
        (let ([id (next-id)])
          (hash-set! hash font id)
          (set-box! accum (cons (font-xml font) (unbox accum)))
          id)))
  
  (consume! empty-font)
  consume!)

; (hashof fill natural) (box (listof fill)) -> (fill -> natural)
(define (make-fill-consumer hash accum)
  ; -> natural
  (define next-id (make-counter 0))
  
  ; fill -> natural
  (define (consume! fill)
    (or (hash-ref hash fill #f)
        (let ([id (next-id)])
          (hash-set! hash fill id)
          (set-box! accum (cons (fill-xml fill) (unbox accum)))
          id)))
  
  (consume! empty-fill)
  (consume! gray-125-fill)
  consume!)

; (hashof border natural) (box (listof border)) -> (border -> natural)
(define (make-border-consumer hash accum)
  ; -> natural
  (define next-id (make-counter 0))
  
  ; border -> natural
  (define (consume! border)
    (or (hash-ref hash border #f)
        (let ([id (next-id)])
          (hash-set! hash border id)
          (set-box! accum (cons (border-xml border) (unbox accum)))
          id)))
  
  (consume! empty-border)
  consume!)

; (hashof style natural) (box (listof style)) -> (style natural natural natural natural -> natural)
(define (make-style-consumer hash accum)
  ; -> natural
  (define next-id (make-counter 0))
  
  ; border -> natural
  (define (consume! style fmt-id font-id fill-id border-id)
    (or (hash-ref hash style #f)
        (let ([id (next-id)])
          (hash-set! hash style id)
          (set-box! accum (cons (style-xml style fmt-id font-id fill-id border-id) (unbox accum)))
          id)))
  
  (consume! empty-style 0 0 0 0)
  consume!)

; XML fragment constructors ----------------------

; number-format natural -> xml
(define (number-format-xml fmt id)
  (xml (numFmt (@ [numFmtId ,id] [formatCode ,(number-format-code fmt)]))))

; font -> xml
(define font-xml
  (match-lambda
    [(struct font (name size color bold italic underline outline shadow strike super sub))
     (xml (font ,(opt-xml name  (name  (@ [val ,name])))
                ,(opt-xml size  (sz    (@ [val ,size])))
                ,(opt-xml color (color (@ [rgb ,(rgba-color-hex color)])))
                ,(opt-xml (eq? bold       #t) (b))
                ,(opt-xml (eq? italic     #t) (i))
                ,(opt-xml (eq? underline  #t) (u))
                ,(opt-xml (eq? outline    #t) (outline))
                ,(opt-xml (eq? shadow     #t) (shadow))
                ,(opt-xml (eq? strike     #t) (strike))
                ,(cond [(eq? super        #t) (xml (vertAlign (@ [val "superscript"])))]
                       [(eq? sub          #t) (xml (vertAlign (@ [val "subscript"])))]
                       [else                  (xml)])))]))

; fill -> xml
(define fill-xml
  (match-lambda
    [(? empty-fill?)
     (xml (fill (patternFill (@ [patternType "none"]))))]
    [(struct pattern-fill (fg bg type))
     (xml (fill (patternFill (@ [patternType ,type])
                             (fgColor (@ [rgb ,(rgba-color-hex fg)]))
                             (bgColor (@ [rgb ,(rgba-color-hex bg)])))))]
    [(struct linear-gradient-fill (angle stops))
     (xml (fill (gradientFill (@ [type "linear"]
                                 [degree ,angle])
                              ,@(map gradient-stop-xml stops))))]
    [(struct path-gradient-fill (top right bottom left stops))
     (xml (fill (gradientFill (@ [type   "path"]
                                 [top    ,(exact->inexact top)]
                                 [right  ,(exact->inexact right)]
                                 [bottom ,(exact->inexact bottom)]
                                 [left   ,(exact->inexact left)])
                              ,@(map gradient-stop-xml stops))))]))

; gradient-stop -> xml
(define gradient-stop-xml
  (match-lambda
    [(struct gradient-stop (pos color))
     (xml (stop (@ [position ,(exact->inexact pos)])
                (color (@ [rgb ,(rgba-color-hex color)]))))]))

; border -> xml
(define border-xml
  (match-lambda
    [(struct border (top right bottom left horizontal vertical diagonal outline-raw diagonal-down-raw diagonal-up-raw))
     (xml (border (@ ,(opt-xml-attr (boolean? outline-raw)       outline      (if outline-raw       "true" "false"))
                     ,(opt-xml-attr (boolean? diagonal-down-raw) diagonalDown (if diagonal-down-raw "true" "false"))
                     ,(opt-xml-attr (boolean? diagonal-up-raw)   diagonalUp   (if diagonal-up-raw   "true" "false")))
                  ,(opt-xml left       (left       (@ [style ,(line-style left)])       ,(line-color-xml left)))
                  ,(opt-xml right      (right      (@ [style ,(line-style right)])      ,(line-color-xml right)))
                  ,(opt-xml top        (top        (@ [style ,(line-style top)])        ,(line-color-xml top)))
                  ,(opt-xml bottom     (bottom     (@ [style ,(line-style bottom)])     ,(line-color-xml bottom)))
                  ,(opt-xml diagonal   (diagonal   (@ [style ,(line-style diagonal)])   ,(line-color-xml diagonal)))
                  ,(opt-xml vertical   (vertical   (@ [style ,(line-style vertical)])   ,(line-color-xml vertical)))
                  ,(opt-xml horizontal (horizontal (@ [style ,(line-style horizontal)]) ,(line-color-xml horizontal)))))]))

; line -> xml
(define (line-color-xml line)
  (let ([hex (rgba-color-hex (line-color line))])
    (opt-xml (not (equal? hex "FF000000"))
      (color (@ [rgb ,(rgba-color-hex (line-color line))])))))

; style natural natural natural natural -> xml
(define (style-xml style numFmtId fontId fillId borderId)
  (match style
    [(struct compiled-style (fmt font fill border align hidden-raw locked-raw))
     (xml (xf (@ ,(opt-xml-attr numFmtId)
                 ,(opt-xml-attr fontId)
                 ,(opt-xml-attr fillId)
                 ,(opt-xml-attr borderId)
                 ,(opt-xml-attr (not (empty-font?      font))   applyFont      "true")
                 ,(opt-xml-attr (not (empty-fill?      fill))   applyFill      "true")
                 ,(opt-xml-attr (not (empty-border?    border)) applyBorder    "true")
                 ,(opt-xml-attr (not (empty-alignment? align))  applyAlignment "true"))
              ,(match align
                 [(? empty-alignment?)
                  (xml)]
                 [(struct alignment (horizontal vertical wrapText shrinkToFit textRotation readingOrder justifyLastLine indent relativeIndent))
                  (xml (alignment (@ ,(opt-xml-attr horizontal)
                                     ,(opt-xml-attr vertical)
                                     ,(opt-xml-attr (boolean? wrapText) wrapText (if wrapText "true" "false"))
                                     ,(opt-xml-attr (boolean? shrinkToFit) shrinkToFit (if shrinkToFit "true" "false"))
                                     ,(opt-xml-attr textRotation)
                                     ,(opt-xml-attr readingOrder readingOrder (reading-order-code readingOrder))
                                     ,(opt-xml-attr (boolean? justifyLastLine) justifyLastLine (if justifyLastLine "true" "false"))
                                     ,(opt-xml-attr indent)
                                     ,(opt-xml-attr relativeIndent))))])
              ,(opt-xml (or (boolean? hidden-raw) (boolean? locked-raw))
                 (protection (@ ,(opt-xml-attr (boolean? hidden-raw) hidden (if hidden-raw "true" "false"))
                                ,(opt-xml-attr (boolean? locked-raw) locked (if locked-raw "true" "false")))))))]))

; Provide statements -----------------------------

(provide/contract
 [styles-xml! (-> cache? workbook? xml?)])
