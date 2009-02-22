#lang scheme/base

(require "test-base.ss")

(require "range.ss"
         "struct.ss")

; Tests ------------------------------------------

(define struct-tests
  (test-suite "struct.ss"
    
    (test-case "range-width"
      (check-equal? (range-width (make-cell #f)) 1)
      (check-equal? (range-width (hc-append #f #f #f)) 3)
      (check-equal? (range-width (vc-append #f #f #f)) 1))
    
    (test-case "range-height"
      (check-equal? (range-height (make-cell #f)) 1)
      (check-equal? (range-height (hc-append #f #f #f)) 1)
      (check-equal? (range-height (vc-append #f #f #f)) 3))
    
    (test-case "range-children"
      (check-equal? (range-children (make-cell #f)) null)
      (check-equal? (range-children (hc-append 1 2 3))
                    (map make-cell '(1 2 3))))
    
    (test-case "part-contains"
      (let ([part (make-part (make-cell #f) 0 0)])
        (check-true  (part-contains? part 0 0))
        (check-false (part-contains? part 1 1)))
      (let ([part (make-part (make-cell #f) 1 1)])
        (check-false (part-contains? part 0 0))
        (check-true  (part-contains? part 1 1)))
      (let ([part (make-part (hc-append #f #f #f) 0 0)])
        (check-true (part-contains? part 0 0))
        (check-true (part-contains? part 1 0))
        (check-true (part-contains? part 2 0)))
      (let* ([parts (union-parts (hc-append #f (vc-append #f #f #f) #f))]
             [pred  (lambda (x y)
                      (ormap (cut part-contains? <> x y) parts))])
        (check-false (pred 0 0))
        (check-true  (pred 1 0))
        (check-false (pred 2 0))
        (check-true  (pred 0 1))
        (check-true  (pred 1 1))
        (check-true  (pred 2 1))
        (check-false (pred 0 2))
        (check-true  (pred 1 2))
        (check-false (pred 2 2))))))

; Provide statements -----------------------------

(provide struct-tests)
