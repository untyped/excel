#lang scheme/base

(require "base.ss")

(require "xml-cache.ss"
         "struct.ss"
         "xml-internal.ss")

; Procedures -------------------------------------

; cache workbook -> listof xml
(define (styles-xml! cache book)
  (let* ([initial-style (make-style)]
         [next-pos      (make-counter 0)]
         [elements      (apply append (for/list ([sheet (in-list (workbook-sheets book))])
                                        (styles-xml/internal! cache (worksheet-data sheet) initial-style next-pos)))])
    (xml (cellXfs (@ [count ,(length elements)])
                  ,@elements))))

; cache range (-> natural) -> (listof xml)
(define (styles-xml/internal! cache range parent-style next-pos)
  
  ; style
  (define style
    (compose-styles parent-style (range-style range)))
  
  ; (U xml #f)
  (define current-xml
    (if (cache-style-ref cache style #f)
        #f
        (style-xml! cache style next-pos)))
  
  ; (listof xml)
  (define child-xmls
    (apply append (for/list ([child (in-list (range-children range))])
                    (styles-xml/internal! cache child style next-pos))))
  
  ; void
  (when (cell? range)
    (cache-cell-style-set! cache range (cache-style-ref cache style)))
  
  ; (listof xml)
  (if current-xml
      (cons current-xml child-xmls)
      child-xmls))

; Helpers ----------------------------------------

; cache style (-> natural) -> xml
(define (style-xml! cache sty next-pos)
  (cache-style-set! cache sty (next-pos))
  (match sty
    [(struct style (fmt font fill border align hidden-raw locked-raw))
     (let* ([numFmtId   (cache-style-ref cache fmt)]
            [fontId     (cache-style-ref cache font)]
            [fillId     (cache-style-ref cache fill)]
            [borderId   (cache-style-ref cache border)])
       (xml (xf (@ ,(opt-xml-attr numFmtId)
                   ,(opt-xml-attr fontId)
                   ,(opt-xml-attr fillId)
                   ,(opt-xml-attr borderId)
                   ,(opt-xml-attr (not (empty-font?   font))   applyFont   "true")
                   ,(opt-xml-attr (not (empty-fill?   fill))   applyFill   "true")
                   ,(opt-xml-attr (not (empty-border? border)) applyBorder "true"))
                ,(opt-xml (or (boolean? hidden-raw) (boolean? locked-raw))
                   (protection (@ ,(opt-xml-attr (boolean? hidden-raw) hidden (if hidden-raw "true" "false"))
                                  ,(opt-xml-attr (boolean? locked-raw) locked (if locked-raw "true" "false")))))
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
                                       ,(opt-xml-attr relativeIndent))))]))))]))

; Provide statements -----------------------------

(provide/contract
 [styles-xml! (-> cache? workbook? xml?)])
