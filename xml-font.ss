#lang scheme/base

(require "base.ss")

(require "xml-cache.ss"
         "struct.ss"
         "xml-internal.ss")

; Procedures -------------------------------------

; cache workbook -> xml
(define (fonts-xml! cache book)
  (let* ([initial-font (make-font)]
         [next-pos     (make-counter 0)]
         [elements     (apply append (for/list ([sheet (in-list (workbook-sheets book))])
                                       (fonts-xml/internal! cache (worksheet-data sheet) initial-font next-pos)))])
    (xml (fonts (@ [count ,(length elements)])
                ,@elements))))

; cache range font (-> natural) -> (listof xml)
(define (fonts-xml/internal! cache range parent-font next-pos)
    
  ; font
  (define font
    (compose-fonts parent-font (style-font (range-style range))))
  
  ; (U xml #f)
  (define current-xml
    (cond [(cache-style-ref cache font #f) #f]
          [else (let* ([pos (next-pos)])
                  (cache-style-set! cache font pos)
                  (xml (font ,(opt-xml (font-name       font) (name (@ [val ,(font-name font)])))
                             ,(opt-xml (font-size       font) (sz   (@ [val ,(font-size font)])))
                             #;,(opt-xml (font-color      font) (color (@ [val ,(font-color font)])))
                             ,(opt-xml (font-bold?      font) (b))
                             ,(opt-xml (font-italic?    font) (i))
                             ,(opt-xml (font-underline? font) (u))
                             ,(opt-xml (font-outline?   font) (outline))
                             ,(opt-xml (font-shadow?    font) (shadow))
                             ,(opt-xml (font-strike?    font) (strike))
                             ,(cond [(font-superscript? font) (xml (vertAlign (@ [val "superscript"])))]
                                    [(font-subscript?   font) (xml (vertAlign (@ [val "subscript"])))]
                                    [else                     (xml)]))))]))
    
    ; (listof xml)
    (define child-xmls
      (apply append (for/list ([child (in-list (range-children range))])
                      (fonts-xml/internal! cache child font next-pos))))
    
    (if current-xml
        (cons current-xml child-xmls)
        child-xmls))

; Provide statements -----------------------------

(provide/contract
 [fonts-xml! (-> cache? workbook? xml?)])
