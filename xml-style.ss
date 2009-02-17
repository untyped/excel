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
    (cond [(cache-style-ref cache style #f) #f]
          [else (let* ([fmt        (style-number-format style)]
                       [font       (style-font style)]
                       [fill       (style-fill style)]
                       [numFmtId   (if (empty-number-format? fmt)
                                       0
                                       (cache-style-ref cache fmt))]
                       [fontId     (cache-style-ref cache font)]
                       [fillId     (cache-style-ref cache fill)]
                       [borderId   #f]
                       [hidden-raw (style-hidden-raw style)]
                       [locked-raw (style-locked-raw style)])
                  (cache-style-set! cache style (next-pos))
                  (xml (xf (@ ,(opt-xml-attr numFmtId)
                              ,(opt-xml-attr fontId)
                              ,(opt-xml-attr fillId)
                              ,(opt-xml-attr borderId)
                              ,(opt-xml-attr (not (empty-font? font)) applyFont "true")
                              ,(opt-xml-attr (not (empty-fill? fill)) applyFill "true"))
                           ,(opt-xml (or (boolean? hidden-raw) 
                                         (boolean? locked-raw))
                              (protection (@ ,(case hidden-raw
                                                [(#t) (xml-attrs [hidden "true"])]
                                                [(#f) (xml-attrs [hidden "false"])]
                                                [else (xml-attrs)])
                                             ,(case locked-raw
                                                [(#t) (xml-attrs [locked "true"])]
                                                [(#f) (xml-attrs [locked "false"])]
                                                [else (xml-attrs)])))))))]))
  
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

; Provide statements -----------------------------

(provide/contract
 [styles-xml! (-> cache? workbook? xml?)])
