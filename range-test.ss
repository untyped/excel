#lang scheme/base

(require "test-base.ss")

(require "range.ss"
         "struct.ss")

; cell ...
(define-values (a b c d e f g h i j)
  (apply values (map make-cell '(a b c d e f g h i j))))

; Tests ------------------------------------------

(define range-tests
  (test-suite "range.ss"
    
    (test-case "foo-append : simple single-cell cases"
      (for ([append-proc (in-list (list hc-append ht-append hb-append))])
        (with-check-info (['append-proc append-proc])
          (check-equal? (append-proc a b c)
                        (make-union (list (make-part a 0 0)
                                          (make-part b 1 0)
                                          (make-part c 2 0))
                                    3 1))))
      (for ([append-proc (in-list (list vc-append vl-append vr-append))])
        (with-check-info (['append-proc append-proc])
          (check-equal? (append-proc a b c)
                        (make-union (list (make-part a 0 0)
                                          (make-part b 0 1)
                                          (make-part c 0 2))
                                    1 3)))))
    
    (test-case "h[ctb]-append"
      (let* ([v-span (vc-append b c d)]
             [v-part (make-part v-span 1 0)])
        (check-equal? (hc-append a v-span e)
                      (make-union (list (make-part a 0 1) v-part (make-part e 2 1)) 3 3))
        (check-equal? (ht-append a v-span e)
                      (make-union (list (make-part a 0 0) v-part (make-part e 2 0)) 3 3))
        (check-equal? (hb-append a v-span e)
                      (make-union (list (make-part a 0 2) v-part (make-part e 2 2)) 3 3))))
    
    (test-case "v[clr]-append"
      (let* ([h-span (hc-append b c d)]
             [h-part (make-part h-span 0 1)])
        (check-equal? (vc-append a h-span e)
                      (make-union (list (make-part a 1 0) h-part (make-part e 1 2)) 3 3))
        (check-equal? (vl-append a h-span e)
                      (make-union (list (make-part a 0 0) h-part (make-part e 0 2)) 3 3))
        (check-equal? (vr-append a h-span e)
                      (make-union (list (make-part a 2 0) h-part (make-part e 2 2)) 3 3))))
    
    (test-case "[lr]-pad"
      (let* ([v-span (vc-append a b c)])
        (check-equal? (l-pad v-span 5) (make-union (list (make-part v-span 5 0)) 6 3))
        (check-equal? (r-pad v-span 5) (make-union (list (make-part v-span 0 0)) 6 3))))
    
    (test-case "[tb]-pad"
      (let* ([h-span (hc-append a b c)])
        (check-equal? (t-pad h-span 5) (make-union (list (make-part h-span 0 5)) 3 6))
        (check-equal? (b-pad h-span 5) (make-union (list (make-part h-span 0 0)) 3 6))))))
        
; Provide statements -----------------------------

(provide range-tests)