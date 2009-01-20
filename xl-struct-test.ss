#lang scheme/base

(require "ref.ss"
         "struct.ss"
         (only-in "struct-internal.ss" worksheet-data)
         "test-base.ss")

(define struct-tests
  (test-suite "struct.ss"
    
    (test-case "make-workbook"
      (check-pred workbook? (make-workbook)))
    
    (test-case "make-worksheet"
      (check-pred worksheet? (make-worksheet "Sheet1")))
    
    (test-case "worksheet-ref, worksheet-set!, worksheet-remove!"
      (define sheet (make-worksheet "Sheet1"))
      
      (define data (worksheet-data sheet))
      
      ; Checks on the empty worksheet:
      
      (check-pred hash? data)
      (check-pred hash-eq? data)
      (check-equal? (hash-count data) 0)
      
      ; Add a cell at A1:
      (let ([a1 (worksheet-set! sheet "A1" (make-cell "OHAI"))])
        (check-equal? (hash-count data) 1)
        (let ([row (hash-ref data 0 #f)])
          (check-pred hash? row)
          (check-equal? (hash-count row) 1)
          (check-eq? (hash-ref row 0 #f) a1)
          (check-eq? (worksheet-ref sheet 0 0) a1)
          (check-eq? (worksheet-ref sheet "A1") a1)
          (check-equal? (cell-value a1) "OHAI")
          (check-equal? (cell-sheet a1) sheet)
          (check-equal? (cell-x a1) 0)
          (check-equal? (cell-y a1) 0))
        
        ; Add a cell at B1:
        (let ([b1 (worksheet-set! sheet "B1" (make-cell "IMINYRSHEET"))])
          (check-equal? (hash-count data) 1)
          (let ([row (hash-ref data 0 #f)])
            (check-pred hash? row)
            (check-equal? (hash-count row) 2)
            (check-eq? (hash-ref row 1 #f) b1)
            (check-eq? (worksheet-ref sheet 1 0) b1)
            (check-eq? (worksheet-ref sheet "B1") b1)
            (check-equal? (cell-value b1) "IMINYRSHEET")
            (check-equal? (cell-sheet b1) sheet)
            (check-equal? (cell-x b1) 1)
            (check-equal? (cell-y b1) 0))
          
          ; Add a cell at A2:
          (let ([a2 (worksheet-set! sheet "A2" (make-cell "KTHXBYE"))])
            (check-equal? (hash-count data) 2)
            (let ([row (hash-ref data 1 #f)])
              (check-pred hash? row)
              (check-equal? (hash-count row) 1)
              (check-eq? (hash-ref row 0 #f) a2)
              (check-eq? (worksheet-ref sheet 0 1) a2)
              (check-eq? (worksheet-ref sheet "A2") a2)
              (check-equal? (cell-value a2) "KTHXBYE")
              (check-equal? (cell-sheet a2) sheet)
              (check-equal? (cell-x a2) 0)
              (check-equal? (cell-y a2) 1))
            
            ; Remove the cell at A1:
            (worksheet-remove! sheet (worksheet-ref sheet "A1"))
            (check-equal? (hash-count data) 2)
            (let ([row0 (hash-ref data 0 #f)]
                  [row1 (hash-ref data 1 #f)])
              (check-pred hash? row0)
              (check-equal? (hash-count row0) 1)
              (check-equal? (hash-ref row0 1) b1)
              (check-equal? (worksheet-ref sheet "B1") b1)
              (check-equal? (hash-ref row1 0) a2)
              (check-equal? (worksheet-ref sheet "A2") a2))
            
            ; Remove the cell at B1:
            (worksheet-remove! sheet (worksheet-ref sheet "B1"))
            (check-equal? (hash-count data) 1)
            (let ([row0 (hash-ref data 0 #f)]
                  [row1 (hash-ref data 1 #f)])
              (check-false row0)
              (check-false (worksheet-ref sheet "B1"))
              (check-equal? (hash-ref row1 0) a2)
              (check-equal? (worksheet-ref sheet "A2") a2))
            
            ; Remove the cell at A2:
            (worksheet-remove! sheet (worksheet-ref sheet "A2"))
            (check-equal? (hash-count data) 0)
            
            (check-false (cell-sheet a1))
            (check-false (cell-x a1))
            (check-false (cell-y a1))
            (check-false (cell-sheet b1))
            (check-false (cell-x b1))
            (check-false (cell-y b1))
            (check-false (cell-sheet a2))
            (check-false (cell-x a2))
            (check-false (cell-y a2))))))
    
    (test-case "worksheet-y-indices, worksheet-x-indices"
      (define sheet (make-worksheet "Sheet1"))
      
      (check-equal? (worksheet-y-indices sheet) null)
      (check-equal? (worksheet-x-indices sheet 0) null)
      
      (worksheet-set! sheet "A1" (make-cell "A1"))
      (worksheet-set! sheet "B1" (make-cell "B1"))
      (worksheet-set! sheet "C1" (make-cell "C1"))
      (worksheet-set! sheet "A2" (make-cell "A2"))
      (worksheet-set! sheet "B2" (make-cell "B2"))
      (worksheet-set! sheet "A3" (make-cell "A3"))
      
      (check-equal? (worksheet-y-indices sheet)   (list 0 1 2))
      (check-equal? (worksheet-x-indices sheet 0) (list 0 1 2))
      (check-equal? (worksheet-x-indices sheet 1) (list 0 1))
      (check-equal? (worksheet-x-indices sheet 2) (list 0))
      (check-equal? (worksheet-x-indices sheet 3) null)
      
      (worksheet-remove! sheet (worksheet-ref sheet "B1"))
      (worksheet-remove! sheet (worksheet-ref sheet "A2"))
      
      (check-equal? (worksheet-y-indices sheet)   (list 0 1 2))
      (check-equal? (worksheet-x-indices sheet 0) (list 0 2))
      (check-equal? (worksheet-x-indices sheet 1) (list 1))
      (check-equal? (worksheet-x-indices sheet 2) (list 0))
      (check-equal? (worksheet-x-indices sheet 3) null)
      
      (worksheet-remove! sheet (worksheet-ref sheet "A1"))
      (worksheet-remove! sheet (worksheet-ref sheet "B2"))
      
      (check-equal? (worksheet-y-indices sheet)   (list 0 2))
      (check-equal? (worksheet-x-indices sheet 0) (list 2))
      (check-equal? (worksheet-x-indices sheet 1) null)
      (check-equal? (worksheet-x-indices sheet 2) (list 0))
      (check-equal? (worksheet-x-indices sheet 3) null))))

; Provide statements -----------------------------

(provide struct-tests)