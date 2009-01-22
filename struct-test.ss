#lang scheme/base

(require "struct.ss"
         "test-base.ss")

(define struct-tests
  (test-suite "struct.ss"
    
    #;(test-case "range-{sheet|x|y|width|height}"
      (let* ([cell  (make-cell "Cell")]
             [union (make-union (list (make-part cell 3 2)) 7 5)]
             [sheet (make-worksheet "Sheet1" union)])
        (check-equal? (range-sheet union) sheet)
        (check-equal? (range-sheet cell) sheet)
        (check-equal? (range-x union) 0)
        (check-equal? (range-x cell) 3)
        (check-equal? (range-y union) 0)
        (check-equal? (range-y cell) 2)
        (check-equal? (range-width union) 7)
        (check-equal? (range-width cell) 1)
        (check-equal? (range-height union) 5)
        (check-equal? (range-height cell) 1)))))

; Provide statements -----------------------------

(provide struct-tests)