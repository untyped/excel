#lang scheme/base

(require "base.ss"
         "xml-cache.ss"
         "struct.ss"
         "xml-internal.ss")

; Procedures -------------------------------------

; cache workbook -> listof xml
(define (number-formats-xml! cache book)
  (let* ([next-id  (make-counter 100)]
         [elements (apply append (for/list ([sheet (in-list (workbook-sheets book))])
                                   (number-formats-xml/internal! cache (worksheet-data sheet) next-id)))])
    (xml (numFmts (@ [count ,(length elements)])
                  ,@elements))))

; cache range (-> natural) -> (listof xml)
(define (number-formats-xml/internal! cache range next-id)
  
  (define fmt
    (let ([style (range-style range)])
      (and style (style-number-format style))))
  
  ; (U xml #f)
  (define current-xml
    (cond [(not fmt) #f]
          [(cache-style-ref cache fmt #f) #f]
          [else (let* ([code        (number-format-code fmt)]
                       [built-in-id (built-in-id code)]
                       [id          (or built-in-id (next-id))])
                  (cache-style-set! cache fmt id)
                  (and (not built-in-id)
                       (xml (numFmt (@ [numFmtId ,id] [formatCode ,code])))))]))
  
  ; (listof xml)
  (define child-xmls
    (apply append (for/list ([child (in-list (range-children range))])
                    (number-formats-xml/internal! cache child next-id))))
  
  (if current-xml
      (cons current-xml child-xmls)
      child-xmls))

; Helpers ----------------------------------------

; string -> (U natural #f)
(define (built-in-id code)
  (if code #f 0))

; Provide statements -----------------------------

(provide/contract
 [number-formats-xml! (-> cache? workbook? xml?)])
