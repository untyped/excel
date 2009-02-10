#lang scheme/base

(require "base.ss"
         "xml-cache.ss"
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
    (style-compose parent-style (range-style range)))
  
  ; (U xml #f)
  (define current-xml
    (cond [(cache-style-ref cache style #f) #f]
          [else (let* ([pos      (next-pos)]
                       [fmt      (style-number-format style)]
                       [font     (style-font style)]
                       [numFmtId (and fmt
                                      (not (number-format-empty? fmt))
                                      (cache-style-ref cache fmt))]
                       [fontId   (and font
                                      (not (font-empty? font))
                                      (cache-style-ref cache font))]
                       [fillId   #f]
                       [borderId #f])
                  (cache-style-set! cache style pos)
                  (xml (xf (@ ,(opt-xml-attr numFmtId)
                              ,(opt-xml-attr fontId)
                              ,(opt-xml-attr fillId)
                              ,(opt-xml-attr borderId)))))]))
  
  ; (listof xml)
  (define child-xmls
    (apply append (for/list ([child (in-list (range-children range))])
                    (styles-xml/internal! cache child style next-pos))))
  
  ; void
  (when (and (cell? range) (not (style-empty? style)))
    (cache-cell-style-set! cache range (cache-style-ref cache style)))
  
  ; (listof xml)
  (if current-xml
      (cons current-xml child-xmls)
      child-xmls))

; Provide statements -----------------------------

(provide/contract
 [styles-xml! (-> cache? workbook? xml?)])
