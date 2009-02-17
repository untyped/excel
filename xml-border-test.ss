#lang scheme/base

(require "test-base.ss")

(require "xml-cache.ss"
         "range.ss"
         "struct.ss"
         "xml-border.ss")

; Test data --------------------------------------

(define border1 (make-border #:top    (make-line)))
(define border2 (make-border #:right  (make-line)))
(define border3 (make-border #:bottom (make-line)))

(define style1 (make-style #:border border1))
(define style2 (make-style #:border border2))
(define style3 (make-style #:border border3))

(define a1 (make-cell "A1"))
(define a2 (make-cell "A2"))
(define b1 (make-cell "B1"))
(define b2 (make-cell "B2"))
(define c1 (make-cell "C1"))
(define c2 (make-cell "C2"))

; Tests ------------------------------------------

(define xml-border-tests
  (test-suite "xml-border.ss"
    
    (test-case "borders-xml! : no borders"
      
      (define range
        (hc-append (vc-append (make-cell "A1")
                              (make-cell "A2"))
                   (vc-append (make-cell "B1")
                              (make-cell "B2"))
                   (vc-append (make-cell "B1")
                              (make-cell "B2"))))
      
      (define book  (make-workbook (list (make-worksheet "Sheet1" range))))
      (define cache (make-cache book))
      (define data  (borders-xml! cache book))
      
      (check-equal?
       (xml->string data)
       (xml->string (xml (borders (@ [count 1])
                                ; Need this unquote to prevent the tag collapsing to <border />:
                                (border ,(xml)))))))
    
    (test-case "borders-xml!"
      
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
        (borders-xml! cache book))
      
      (check-eq?   (cache-style-ref cache border1) 1)
      (check-false (cache-style-ref cache border2 #f))
      (check-false (cache-style-ref cache border3 #f))
      (check-eq?   (cache-style-ref cache (compose-borders border1 border2)) 2)
      (check-eq?   (cache-style-ref cache (compose-borders border1 border3)) 3)
      
      (check-equal?
       (xml->string data)
       (xml->string (xml (borders (@ [count 4])
                                  (border ,(xml))
                                  (border (top    (@ [style "thin"]) ,(xml)))
                                  (border (top    (@ [style "thin"]) ,(xml))
                                          (right  (@ [style "thin"]) ,(xml)))
                                  (border (top    (@ [style "thin"]) ,(xml))
                                          (bottom (@ [style "thin"]) ,(xml))))))))))

; Provide statements -----------------------------

(provide xml-border-tests)
