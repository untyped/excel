#lang scheme/base

(require "test-base.ss")

(require "xml-cache.ss"
         "range.ss"
         "struct.ss"
         "xml-font.ss"
         "xml-number-format.ss"
         "xml-style.ss")

; Test data --------------------------------------

(define fmt1 (make-number-format #f))
(define fmt2 (make-number-format "0"))
(define fmt3 (make-number-format "0.00"))

(define style1 (make-style #:number-format fmt1))
(define style2 (make-style #:number-format fmt2))
(define style3 (make-style #:number-format fmt3))

(define a1 (make-cell "A1"))
(define a2 (make-cell "A2"))
(define b1 (make-cell "B1"))
(define b2 (make-cell "B2"))
(define c1 (make-cell "C1"))
(define c2 (make-cell "C2"))

; Tests ------------------------------------------

(define xml-style-tests
  (test-suite "xml-style.ss"
    
    (test-case "styles-xml!"
      
      (define range
        (hc-append (hc-append (vc-append a1 a2 #:style style2)
                              (vc-append b1 (hc-append b2 #:style style3))
                              #:style style1)
                   (vc-append c1 c2)))
      
      (define book
        (make-workbook (list (make-worksheet "Sheet1" range))))
      
      (define cache
        (make-cache book))
      
      (define data
        (begin
          (number-formats-xml! cache book)
          (fonts-xml! cache book)
          (styles-xml! cache book)))
      
      (check-eq? (cache-style-ref cache style1) 0)
      (check-eq? (cache-style-ref cache style2) 1)
      (check-eq? (cache-style-ref cache style3) 2)
      
      (check-equal?
       (xml->string data)
       (xml->string (xml (cellStyleXfs (@ [count 4])
                                       (xf)
                                       (xf (@ [numFmtId 0]))
                                       (xf (@ [numFmtId 100]))
                                       (xf (@ [numFmtId 101]))))))
      
      (check-eq? (cache-cell-style-ref cache a1) (cache-style-ref cache style2))
      (check-eq? (cache-cell-style-ref cache a2) (cache-style-ref cache style2))
      (check-eq? (cache-cell-style-ref cache b1) (cache-style-ref cache style1))
      (check-eq? (cache-cell-style-ref cache b2) (cache-style-ref cache style3))
      (check-false (cache-cell-style-ref cache c1))
      (check-false (cache-cell-style-ref cache c2)))))

; Provide statements -----------------------------

(provide xml-style-tests)
