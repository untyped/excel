#lang scheme/base

(require "xml-cache.ss"
         "range.ss"
         "struct.ss"
         "test-base.ss"
         "xml-number-format.ss")

; Tests ------------------------------------------

(define xml-number-format-tests
  (test-suite "xml-number-format.ss"
    
    (test-case "number-formats-xml!"
      
      (define fmt1 (make-number-format #f))
      (define fmt2 (make-number-format "0"))
      (define fmt3 (make-number-format "0.00"))
      
      (define style1 (make-style fmt1))
      (define style2 (make-style fmt2))
      (define style3 (make-style fmt3))
      
      (define range
        (hc-append (vc-append (make-cell "A1")
                              (make-cell "A2")
                              (make-cell "A3")
                              #:style style2)
                   (vc-append (make-cell "B1")
                              (make-cell "B2")
                              (make-cell "B3")
                              #:style style3)
                   #:style style1))
      
      (define book
        (make-workbook (list (make-worksheet "Sheet1" range))))
      
      (define cache
        (make-cache book))
      
      (define data
        (number-formats-xml! cache book))
      
      (check-eq? (cache-style-ref cache fmt1) 0)
      (check-eq? (cache-style-ref cache fmt2) 100)
      (check-eq? (cache-style-ref cache fmt3) 101)
      
      (check-equal?
       (xml->string data)
       (xml->string (xml (numFmts (@ [count 2])
                                  (numFmt (@ [numFmtId 100] [formatCode "0"]))
                                  (numFmt (@ [numFmtId 101] [formatCode "0.00"])))))))))

; Provide statements -----------------------------

(provide xml-number-format-tests)
