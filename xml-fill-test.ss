#lang scheme/base

(require "test-base.ss")

(require "xml-cache.ss"
         "range.ss"
         "struct.ss"
         "xml-fill.ss")

; Tests ------------------------------------------

(define xml-fill-tests
  (test-suite "xml-fill.ss"
    
    (test-case "fills-xml!"
      
      (define fill1 empty-fill)
      (define fill2 (make-solid-fill (make-rgba-color 0 0 0 0)))
      (define fill3 (make-solid-fill (make-rgba-color 1 1 1 1)))
      
      (define style1 (make-style #:fill fill1))
      (define style2 (make-style #:fill fill2))
      (define style3 (make-style #:fill fill3))
      
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
        (fills-xml! cache book))
      
      (check-eq? (cache-style-ref cache fill1)         0)
      (check-eq? (cache-style-ref cache empty-fill)    0)
      (check-eq? (cache-style-ref cache gray-125-fill) 1)
      (check-eq? (cache-style-ref cache fill2)         2)
      (check-eq? (cache-style-ref cache fill3)         3)
      
      (check-equal?
       (xml->string data)
       (xml->string (xml (fills (@ [count 4])
                                (fill (patternFill (@ [patternType "none"])))
                                (fill (patternFill (@ [patternType "gray125"])
                                                   (fgColor (@ [rgb "FF000000"]))
                                                   (bgColor (@ [rgb "FFFFFFFF"]))))
                                (fill (patternFill (@ [patternType "solid"])
                                                   (fgColor (@ [rgb "00000000"]))
                                                   (bgColor (@ [rgb "00000000"]))))
                                (fill (patternFill (@ [patternType "solid"])
                                                   (fgColor (@ [rgb "FFFFFFFF"]))
                                                   (bgColor (@ [rgb "FFFFFFFF"])))))))))))

; Provide statements -----------------------------

(provide xml-fill-tests)
