#lang scheme/base

(require "base.ss")

(require "xml-cache.ss"
         "struct.ss"
         "xml-internal.ss")

; Procedures -------------------------------------

; cache workbook -> xml
(define (borders-xml! cache book)
  (let* ([next-pos (make-counter 0)]
         [elements (list* (border-xml! cache empty-border next-pos)
                          (apply append (for/list ([sheet (in-list (workbook-sheets book))])
                                          (borders-xml/internal! cache (worksheet-data sheet) empty-border next-pos))))])
    (xml (borders (@ [count ,(length elements)])
                  ,@elements))))

; cache range border (-> natural) -> (listof xml)
(define (borders-xml/internal! cache range parent-border next-pos)
  
  ; border
  (define border
    (compose-borders parent-border (style-border (range-style range))))
  
  ; (U xml #f)
  (define current-xml
    (if (cache-style-ref cache border #f)
        #f
        (border-xml! cache border next-pos)))
  
  ; (listof xml)
  (define child-xmls
    (apply append (for/list ([child (in-list (range-children range))])
                    (borders-xml/internal! cache child border next-pos))))
  
  (if current-xml
      (cons current-xml child-xmls)
      child-xmls))

; Helpers ----------------------------------------

; cache border (-> natural) -> xml
(define (border-xml! cache bord next-pos)
  (cache-style-set! cache bord (next-pos))
  (match bord
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

; Provide statements -----------------------------

(provide/contract
 [borders-xml! (-> cache? workbook? xml?)])
