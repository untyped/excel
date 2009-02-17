#lang scheme/base

(require "base.ss")

(require "xml-cache.ss"
         "struct.ss"
         "xml-internal.ss")

; Procedures -------------------------------------

; cache workbook -> xml
(define (fills-xml! cache book)
  (let* ([next-pos (make-counter 0)]
         ; Record reserved styles before we traverse the tree:
         [elements (list* (fill-xml! cache empty-fill next-pos)
                          (fill-xml! cache gray-125-fill next-pos)
                          (apply append (for/list ([sheet (in-list (workbook-sheets book))])
                                          (fills-xml/internal! cache (worksheet-data sheet) empty-fill next-pos))))])
    (xml (fills (@ [count ,(length elements)])
                ,@elements))))

; cache range fill (-> natural) -> (listof xml)
(define (fills-xml/internal! cache range parent-fill next-pos)
  
  (define fill
    (compose-fills parent-fill (style-fill (range-style range))))
  
  ; (U xml #f)
  (define current-xml
    (if (cache-style-ref cache fill #f)
        #f
        (fill-xml! cache fill next-pos)))
  
  ; (listof xml)
  (define child-xmls
    (apply append (for/list ([child (in-list (range-children range))])
                    (fills-xml/internal! cache child fill next-pos))))
  
  (if current-xml
      (cons current-xml child-xmls)
      child-xmls))

(define gradient-stop-xml
  (match-lambda
    [(struct gradient-stop (pos color))
     (xml (stop (@ [position ,(exact->inexact pos)])
                (color (@ [rgb ,(rgba-color-hex color)]))))]))

; Helpers ----------------------------------------

; string -> (U natural #f)
(define (built-in-id code)
  (if code #f 0))

; cache fill (-> natural) -> xml
(define (fill-xml! cache fil next-pos)
  (cache-style-set! cache fil (next-pos))
  (match fil
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

; Provide statements -----------------------------

(provide/contract
 [fills-xml! (-> cache? workbook? xml?)])
