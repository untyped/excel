#lang scheme/base

(require "test-base.ss")

(require (unlib-in hash)
         "xml-cache.ss"
         "range.ss"
         "struct.ss")

(require/expose "xml-cache.ss"
  (create-forward-lookup/book
   create-forward-lookup/sheet
   create-forward-lookup/range
   create-reverse-lookup))

; cell ...
(define-values (a b c d e f g h i j)
  (apply values (map make-cell '(a b c d e f g h i j))))

; Tests ------------------------------------------

(define cache-tests
  (test-suite "xml-cache.ss"
    
    (test-equal? "create-forward-lookup/range"
      (create-forward-lookup/range (hc-append a (vc-append b c d) e) 0 0)
      `((0 . ((1 . ,b)))
        (1 . ((0 . ,a) (1 . ,c) (2 . ,e)))
        (2 . ((1 . ,d)))))
    
    (test-equal? "create-forward-lookup/sheet"
      (create-forward-lookup/sheet (make-worksheet "Sheet1" (vc-append b (hc-append a c e) d)))
      `((0 . ((1 . ,b)))
        (1 . ((0 . ,a) (1 . ,c) (2 . ,e)))
        (2 . ((1 . ,d)))))
    
    (test-case "create-forward-lookup/book"
      ; Sheet names are deliberately in the wrong order:
      (let* ([first (make-worksheet "Sheet2" (vc-append a b c))]
             [last  (make-worksheet "Sheet1" (hc-append d e f))]
             [book  (make-workbook (list first last))])
        (create-forward-lookup/book book)
        `((,first . ((0 . ((0 . ,a)))
                     (1 . ((0 . ,b)))
                     (2 . ((0 . ,c)))))
          (,last  . ((0 . ((0 . ,d) (1 . ,e) (2 . ,f))))))))
    
    (test-case "create-reverse-lookup"
      (let* ([first (make-worksheet "Sheet2" (vc-append a b c))]
             [last  (make-worksheet "Sheet1" (hc-append d e f))])
        (check-equal?
         (create-reverse-lookup
          `((,first . ((0 . ((0 . ,a)))
                       (1 . ((0 . ,b)))
                       (2 . ((0 . ,c)))))
            (,last  . ((0 . ((0 . ,d) (1 . ,e) (2 . ,f)))))))
         (make-hasheq/alist
          `((,a . (,first 0 0))
            (,b . (,first 0 1))
            (,c . (,first 0 2))
            (,d . (,last 0 0))
            (,e . (,last 1 0))
            (,f . (,last 2 0)))))))

    (test-case "cache-ref"
       (let* ([first (make-worksheet "Sheet2" (vc-append a b c))]
              [last  (make-worksheet "Sheet1" (hc-append d e f))]
              [book  (make-workbook (list first last))]
              [cache (make-cache book)])
         (check-equal? (cache-ref cache first a) "A1")
         (check-equal? (cache-ref cache first a #t #f) "$A1")
         (check-equal? (cache-ref cache first a #f #t) "A$1")
         (check-equal? (cache-ref cache first f #t #f) "Sheet1!$C1")
         (check-equal? (cache-ref cache first f #f #t) "Sheet1!C$1")))))

; Provide statements -----------------------------

(provide cache-tests)