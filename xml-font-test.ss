#lang scheme/base

(require "test-base.ss")

(require "xml-cache.ss"
         "range.ss"
         "struct.ss"
         "xml-font.ss")

; Test data --------------------------------------

(define font1 (make-font #:name "Dave" #:bold? #t))
(define font2 (make-font #:bold? #f))
(define font3 (make-font #:italic? #t))

(define style1 (make-style #:font font1))
(define style2 (make-style #:font font2))
(define style3 (make-style #:font font3))

(define a1 (make-cell "A1"))
(define a2 (make-cell "A2"))
(define b1 (make-cell "B1"))
(define b2 (make-cell "B2"))
(define c1 (make-cell "C1"))
(define c2 (make-cell "C2"))

; Tests ------------------------------------------

(define xml-font-tests
  (test-suite "xml-font.ss"
    
    (test-case "fonts-xml! : no fonts"
      
      (define range
        (hc-append (vc-append (make-cell "A1")
                              (make-cell "A2"))
                   (vc-append (make-cell "B1")
                              (make-cell "B2"))
                   (vc-append (make-cell "B1")
                              (make-cell "B2"))))
      
      (define book  (make-workbook (list (make-worksheet "Sheet1" range))))
      (define cache (make-cache book))
      (define data  (fonts-xml! cache book))
      
      (check-equal?
       (xml->string data)
       (xml->string (xml (fonts (@ [count 1])
                                ; Need this unquote to prevent the tag collapsing to <font />:
                                (font ,(xml)))))))
    
    (test-case "fonts-xml!"
      
      (define range
        (hc-append (vc-append (make-cell "A1")
                              (make-cell "A2"))
                   (vc-append (make-cell "B1")
                              (make-cell "B2")
                              #:style style2)
                   (vc-append (make-cell "B1")
                              (make-cell "B2")
                              #:style style3)
                   #:style style1))
      
      (define book
        (make-workbook (list (make-worksheet "Sheet1" range))))
      
      (define cache
        (make-cache book))
      
      (define data
        (fonts-xml! cache book))
      
      (check-eq?   (cache-style-ref cache font1) 0)
      (check-false (cache-style-ref cache font2 #f))
      (check-false (cache-style-ref cache font3 #f))
      (check-eq?   (cache-style-ref cache (font-compose font1 font2)) 1)
      (check-eq?   (cache-style-ref cache (font-compose font1 font3)) 2)
      
      (check-equal?
       (xml->string data)
       (xml->string (xml (fonts (@ [count 3])
                                (font (name (@ [val "Dave"])) (b))
                                (font (name (@ [val "Dave"])))
                                (font (name (@ [val "Dave"])) (b) (i)))))))))

; Provide statements -----------------------------

(provide xml-font-tests)
