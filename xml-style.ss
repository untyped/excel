#lang scheme/base

(require "base.ss")

(require scheme/math
         "struct.ss"
         "xml-cache.ss"
         "xml-internal.ss")

; Procedures -------------------------------------

; cache workbook -> xml
(define (stylesheet-xml! cache book)
  
  ; (box (listof xml))
  (define fmt-accum              (box null))
  (define font-accum             (box null))
  (define fill-accum             (box null))
  (define border-accum           (box null))
  (define style-accum            (box null))
  (define diff-style-accum       (box null))
  
  ; number-format -> natural
  (define consume-number-format!
    (make-number-format-consumer cache fmt-accum))
  
  ; font -> natural
  (define consume-font!
    (make-font-consumer cache font-accum))
  
  ; fill -> natural
  (define consume-fill!
    (make-fill-consumer cache fill-accum))
  
  ; border -> natural
  (define consume-border!
    (make-border-consumer cache border-accum))
  
  ; style natural natural -> compiled-style natural
  (define consume-style!
    (let ([consume-style/internal! (make-style-consumer cache style-accum)])
      (lambda (style0 x y)
        (let ([style (compile-style style0 x y)])
          (values
           style
           (consume-style/internal!
            style
            (consume-number-format! (compiled-style-number-format style))
            (consume-font!          (compiled-style-font          style))
            (consume-fill!          (compiled-style-fill          style))
            (consume-border!        (compiled-style-border        style))))))))
  
  ; compiled-style -> natural
  (define consume-diff-style!
    (make-diff-style-consumer cache diff-style-accum))
  
  ; range worksheet compiled-style natural natural -> void
  ;
  ; Multiply the string length of the printed value of the cell
  ; by these values to get the suggested column width:
  (define (cache-cell-dimensions! range sheet style x y)
    (match (cell-dimensions range)
      [(struct cell-dims (min-width max-width min-height max-height hide-row? hide-column?))
       (when (or min-width max-width)
         (cache-col-width-set! cache sheet x (constrain (or (cache-col-width-ref cache sheet x) 10) min-width max-width)))
       (when (or min-height max-height)
         (cache-row-height-set! cache sheet y (constrain (or (cache-row-height-ref cache sheet y) 1) min-height max-height)))
       (when hide-column? (cache-col-visibility-set! cache sheet x #f))
       (when hide-row?    (cache-row-visibility-set! cache sheet y #f))]
      [#f (void)]))
  
  (for ([sheet (in-list (workbook-sheets book))])
    ; Traverse the wokrsheet tree accumulating:
    ;   - "style" mappings   : (U number-format font fill border compiled-style) -> natural
    ;   - "value" mappings   : sheet x y -> (U cell #f) style-id
    ;   - "address" mappings : cell -> sheet x y
    ; 
    ; We need all of this information to render the worksheet data and conditional formatting rules later.
    (range-for-each
     ; range
     (worksheet-data sheet)
     ; compose/range : range style natural natural -> style
     (lambda (range style x y)
       (compose-styles style (range-style range)))
     ; compose/part  : part style -> style
     (lambda (part style)
       (translate-style style (part-dx part) (part-dy part)))
     ; consume!      : range style natural natural -> void
     (lambda (range style x0 y0)
       ; Unions: cache style and value mappings for empty (x,y) coords:
       (when (union? range)
         (let ([parts (union-parts range)])
           (for* ([y (in-range (range-height range))]
                  [x (in-range (range-width  range))])
                 (unless (ormap (cut part-contains? <> x y) parts)
                   (let-values ([(style style-id) (consume-style! style x y)])
                     (unless (zero? style-id)
                       (cache-value-set! cache sheet (+ x0 x) (+ y0 y) #f style-id)))))))
       ; Cells: cache style, value, address and column width mappings:
       (when (cell? range)
         (let-values ([(style style-id) (consume-style! style 0 0)])
           (cache-value-set! cache sheet x0 y0 range style-id)
           (cache-address-set! cache range sheet x0 y0)
           (cache-cell-dimensions! range sheet style x0 y0)))
       ; Unions and cells: cache style mappings for diff styles:
       (for ([cf (in-list (range-conditional-formats range))])
         (consume-diff-style! (conditional-format-style cf))))
     ; accum0        : style
     empty-style))
  
  ; xml
  (xml ,standalone-header-xml
       (styleSheet (@ [xmlns ,spreadsheetml-namespace])
                   (numFmts (@ [count ,(length (unbox fmt-accum))])
                            ,@(reverse (unbox fmt-accum)))
                   (fonts   (@ [count ,(length (unbox font-accum))])
                            ,@(reverse (unbox font-accum)))
                   (fills   (@ [count ,(length (unbox fill-accum))])
                            ,@(reverse (unbox fill-accum)))
                   (borders (@ [count ,(length (unbox border-accum))])
                            ,@(reverse (unbox border-accum)))
                   (cellXfs (@ [count ,(length (unbox style-accum))])
                            ,@(reverse (unbox style-accum)))
                   (dxfs    (@ [count ,(length (unbox diff-style-accum))])
                            ,@(reverse (unbox diff-style-accum))))))

; Style element consumers ------------------------

; cache (box (listof number-format)) -> (number-format -> natural)
(define (make-number-format-consumer cache accum)
  ; -> natural
  (define next-id (make-counter 100))
  
  ; number-format -> natural
  (define (consume! fmt)
    (or (cache-style-ref cache fmt #f)
        (let ([id (next-id)])
          (cache-style-set! cache fmt id)
          (set-box! accum (cons (number-format-xml fmt id) (unbox accum)))
          id)))
  
  (cache-style-set! cache empty-number-format 0)
  consume!)

; cache (box (listof font)) -> (font -> natural)
(define (make-font-consumer cache accum)
  ; -> natural
  (define next-id (make-counter 0))
  
  ; font -> natural
  (define (consume! font)
    (or (cache-style-ref cache font #f)
        (let ([id (next-id)])
          (cache-style-set! cache font id)
          (set-box! accum (cons (font-xml font) (unbox accum)))
          id)))
  
  (consume! empty-font)
  consume!)

; cache (box (listof fill)) -> (fill -> natural)
(define (make-fill-consumer cache accum)
  ; -> natural
  (define next-id (make-counter 0))
  
  ; fill -> natural
  (define (consume! fill)
    (or (cache-style-ref cache fill #f)
        (let ([id (next-id)])
          (cache-style-set! cache fill id)
          (set-box! accum (cons (fill-xml fill) (unbox accum)))
          id)))
  
  (consume! empty-fill)
  (consume! gray-125-fill)
  consume!)

; cache (box (listof border)) -> (border -> natural)
(define (make-border-consumer cache accum)
  ; -> natural
  (define next-id (make-counter 0))
  
  ; border -> natural
  (define (consume! border)
    (or (cache-style-ref cache border #f)
        (let ([id (next-id)])
          (cache-style-set! cache border id)
          (set-box! accum (cons (border-xml border) (unbox accum)))
          id)))
  
  (consume! empty-border)
  consume!)

; cache (box (listof style)) -> (style natural natural natural natural -> natural)
(define (make-style-consumer cache accum)
  ; -> natural
  (define next-id (make-counter 0))
  
  ; border -> natural
  (define (consume! style fmt-id font-id fill-id border-id)
    (or (cache-style-ref cache style #f)
        (let ([id (next-id)])
          (cache-style-set! cache style id)
          (set-box! accum (cons (style-xml style fmt-id font-id fill-id border-id)
                                (unbox accum)))
          id)))
  
  (consume! empty-style 0 0 0 0)
  consume!)

; cache (box (listof style)) -> (style -> natural)
(define (make-diff-style-consumer cache accum)
  ; -> natural
  (define next-id (make-counter 0))
  
  ; border -> natural
  (define (consume! style)
    (or (cache-diff-style-ref cache style #f)
        (let ([id (next-id)])
          (cache-diff-style-set! cache style id)
          (set-box! accum (cons (diff-style-xml style) (unbox accum)))
          id)))
  
  consume!)

; XML fragment constructors ----------------------

; number-format [(U natural #f)] -> xml
(define (number-format-xml fmt [numFmtId #f])
  (xml (numFmt (@ ,(opt-xml-attr numFmtId)
                  [formatCode ,(number-format-code fmt)]))))

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

; alignment -> xml
(define alignment-xml
  (match-lambda
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
                        ,(opt-xml-attr relativeIndent))))]))

; (U boolean void) (U boolean void) -> xml
(define (protection-xml hidden-raw locked-raw)
  (opt-xml (or (boolean? hidden-raw) (boolean? locked-raw))
    (protection (@ ,(opt-xml-attr (boolean? hidden-raw) hidden (if hidden-raw "true" "false"))
                   ,(opt-xml-attr (boolean? locked-raw) locked (if locked-raw "true" "false"))))))

; compiled-style natural natural natural natural -> xml
(define (style-xml style numFmtId fontId fillId borderId)
  (match style
    [(struct compiled-style (fmt font fill border align hidden-raw locked-raw))
     (xml (xf (@ ,(opt-xml-attr numFmtId)
                 ,(opt-xml-attr fontId)
                 ,(opt-xml-attr fillId)
                 ,(opt-xml-attr borderId)
                 ,(opt-xml-attr (not (empty-number-format? fmt))    applyNumberFormat "true")
                 ,(opt-xml-attr (not (empty-font?          font))   applyFont         "true")
                 ,(opt-xml-attr (not (empty-fill?          fill))   applyFill         "true")
                 ,(opt-xml-attr (not (empty-border?        border)) applyBorder       "true")
                 ,(opt-xml-attr (not (empty-alignment?     align))  applyAlignment    "true")
                 ,(opt-xml-attr (or (boolean? hidden-raw) (boolean? locked-raw))
                    applyProtection "true"))
              ,(alignment-xml align)
              ,(protection-xml hidden-raw locked-raw)))]))

; compiled-style -> xml
(define (diff-style-xml style)
  (match style
    [(struct compiled-style (fmt font fill border align hidden-raw locked-raw))
     (xml (dxf ,(opt-xml (not (empty-font? font))
                  ,(font-xml font))
               ,(opt-xml (not (empty-number-format? fmt))
                  ,(number-format-xml fmt))
               ,(opt-xml (not (empty-fill? fill))
                  ,(fill-xml fill))
               ,(opt-xml (not (empty-alignment? align))
                  ,(alignment-xml align))
               ,(opt-xml (not (empty-border? border))
                  ,(border-xml border))
               ,(opt-xml (or (boolean? hidden-raw) (boolean? locked-raw))
                  ,(protection-xml hidden-raw locked-raw))))]))

; Helpers ----------------------------------------

; (_ number (U number #f) (U number #f)) -> number
(define-syntax-rule (constrain *val* *min-val* *max-val*)
  (let ([val     *val*]
        [min-val *min-val*]
        [max-val *max-val*])
    (if min-val
        (if max-val
            (max min-val (min max-val val))
            (max min-val val))
        (if max-val
            (min max-val val)
            val))))

; Provide statements -----------------------------

(provide/contract
 [stylesheet-xml! (-> cache? workbook? xml?)])
