#lang scheme/base

(require "test-base.ss")

(require (only-in srfi/1 iota)
         "range.ss"
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
        (check-false (pred 2 2))))
    
    (test-case "make-function"
      (parameterize ([max-function-arity         #f]
                     [max-function-nesting-depth #f])
        (check-not-exn
          (lambda ()
            (apply make-function '+ (iota 100))))
        (check-not-exn
          (lambda ()
            (let loop ([depth 10])
              (if (zero? depth)
                  1
                  (apply make-function 'not (list (loop (sub1 depth)))))))))
      (parameterize ([max-function-arity         3]
                     [max-function-nesting-depth 3])
        (check-not-exn
          (lambda ()
            (apply make-function '+ (iota 3))))
        (check-exn exn:fail?
          (lambda ()
            (apply make-function '+ (iota 4))))
        (check-not-exn
          (lambda ()
            (let loop ([depth 3])
              (if (zero? depth)
                  1
                  (apply make-function 'not (list (loop (sub1 depth))))))))
        (check-exn exn:fail?
          (lambda ()
            (let loop ([depth 4])
              (if (zero? depth)
                  1
                  (apply make-function 'not (list (loop (sub1 depth))))))))))
    
    (test-case "optimize-commutative-function"
      (define (opt . args) (apply optimize-commutative-function '+ args))
      (define (add . args) (apply make-function '+ args))
      
      (parameterize ([max-function-arity #f])
        (check-equal? (opt) (add))
        (check-equal? (apply opt (iota 100)) (apply add (iota 100))))
      
      (parameterize ([max-function-arity 3])
        (check-equal? (opt)
                      (add))
        (check-equal? (opt 1 2 3 4 5)
                      (add (add 1 2 3) 4 5))
        (check-equal? (opt 1 2 3 4 5 6)
                      (add (add 1 2 3)
                           (add 4 5 6)))
        (check-equal? (opt 1 2 3 4 5 6 7 8 9 10)
                      (add (add (add 1 2 3)
                                (add 4 5 6)
                                (add 7 8 9))
                           10))
        (check-equal? (opt 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                      (add (add (add 1 2 3)
                                (add 4 5 6)
                                (add 7 8 9))
                           (add 10 11 12)
                           (add 13 14 15)))
        (check-equal? (opt 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)
                      (add (add (add 1 2 3)
                                (add 4 5 6)
                                (add 7 8 9))
                           (add (add 10 11 12)
                                (add 13 14 15)
                                (add 16 17 18))))))))

; Provide statements -----------------------------

(provide struct-tests)
