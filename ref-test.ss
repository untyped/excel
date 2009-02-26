#lang scheme/base

(require "test-base.ss")

(require "range.ss"
         "ref.ss"
         "struct.ss")

(define ref-tests
  (test-suite "ref.ss"
    
    (test-case "x->col"
      (check-equal? (x->col 0)  "A")
      (check-equal? (x->col 25) "Z")
      (check-equal? (x->col 26) "AA")
      (check-equal? (x->col 51) "AZ")
      (check-equal? (x->col 52) "BA")
      (check-equal? (x->col (+ (* 26 26) 25)) "ZZ")
      (check-equal? (x->col (* 26 27)) "AAA"))
    
    (test-case "y->row"
      (check-equal? (y->row 0)   "1")
      (check-equal? (y->row 99)  "100")
      (check-equal? (y->row 100) "101"))
    
    (test-case "col->x"
      (for ([x (in-range 0 1000)])
        (check-equal? (col->x (x->col x)) x)))
    
    (test-case "row->y"
      (for ([y (in-range 0 1000)])
        (check-equal? (row->y (y->row y)) y)))
    
    (test-case "xy->ref"
      (check-equal? (xy->ref 0 0) "A1")
      (check-equal? (xy->ref 0 0 #t #f) "$A1")
      (check-equal? (xy->ref 0 0 #f #t) "A$1")
      (check-equal? (xy->ref 0 0 #t #t) "$A$1")
      (check-equal? (xy->ref 26 26 #t #t) "$AA$27"))
    
    (test-case "sheet+xy->ref"
      (check-equal? (sheet+xy->ref #f 0 0) "A1")
      (check-equal? (sheet+xy->ref (make-worksheet "Sheet1" (make-cell 123)) 0 0)
                    "Sheet1!A1")
      (check-equal? (sheet+xy->ref (make-worksheet "Sheet1" (make-cell 123)) 26 26 #t #f)
                    "Sheet1!$AA27"))
    
    (test-case "ref->xy"
      (check-equal? (call-with-values (cut ref->xy "A2") list) (list 0 1))
      (check-equal? (call-with-values (cut ref->xy "a2") list) (list 0 1))
      (check-equal? (call-with-values (cut ref->xy "$A$2") list) (list 0 1))
      (check-equal? (call-with-values (cut ref->xy "Sheet1!$A$2") list) (list 0 1)))
    
    (test-case "ref->sheet+xy"
      (check-equal? (call-with-values (cut ref->sheet+xy "A2") list) (list #f 0 1))
      (check-equal? (call-with-values (cut ref->sheet+xy "a2") list) (list #f 0 1))
      (check-equal? (call-with-values (cut ref->sheet+xy "$A$2") list) (list #f 0 1))
      (check-equal? (call-with-values (cut ref->sheet+xy "Sheet1!$A$2") list)
                    (list "Sheet1" 0 1)))
    
    
    (test-case "range-address"
      (check-equal? (range-address (make-cell 123) 2 1) "C2")
      (check-equal? (range-address (hc-append (make-cell 123) (make-cell 123)) 2 1) "C2:D2")
      (check-equal? (range-address (vc-append (make-cell 123) (make-cell 123)) 2 1) "C2:C3"))))

; Provide statements -----------------------------

(provide ref-tests)