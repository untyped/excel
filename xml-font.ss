#lang scheme/base

(require "base.ss")

(require "xml-cache.ss"
         "struct.ss"
         "xml-internal.ss")

; Procedures -------------------------------------

; cache workbook -> xml
(define (fonts-xml! cache book)
  (let* ([next-pos (make-counter 0)]
         [elements (list* (font-xml! cache empty-font next-pos)
                          (apply append (for/list ([sheet (in-list (workbook-sheets book))])
                                          (fonts-xml/internal! cache (worksheet-data sheet) empty-font next-pos))))])
    (xml (fonts (@ [count ,(length elements)])
                ,@elements))))

; cache range font (-> natural) -> (listof xml)
(define (fonts-xml/internal! cache range parent-font next-pos)
  
  ; font
  (define font
    (compose-fonts parent-font (style-font (range-style range))))
  
  ; (U xml #f)
  (define current-xml
    (if (cache-style-ref cache font #f)
        #f
        (font-xml! cache font next-pos)))
  
  ; (listof xml)
  (define child-xmls
    (apply append (for/list ([child (in-list (range-children range))])
                    (fonts-xml/internal! cache child font next-pos))))
  
  (if current-xml
      (cons current-xml child-xmls)
      child-xmls))

; Helpers ----------------------------------------

; cache font (-> natural) -> xml
(define (font-xml! cache fnt next-pos)
  (cache-style-set! cache fnt (next-pos))
  (match fnt
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

; Provide statements -----------------------------

(provide/contract
 [fonts-xml! (-> cache? workbook? xml?)])
