#lang scheme/base

(require "test-base.ss")

(require "struct.ss")

(define struct-tests
  (test-suite "struct.ss"
    
    (test-case "empty-style?"
      (check-true (empty-style? (make-style)))
      (check-false (empty-style? (make-style #:number-format general-number-format))))
    
    (test-case "compose-styles"
      (let ([fmt1 (make-number-format #f)]
            [fmt2 (make-number-format "0")])
        (check-equal? (compose-styles (make-style #:number-format fmt1)
                                      (make-style #:number-format fmt2))
                      (make-style #:number-format fmt2))
        (check-equal? (compose-styles (make-style)
                                      (make-style #:number-format fmt2))
                      (make-style #:number-format fmt2))
        (check-equal? (compose-styles (make-style #:number-format fmt1)
                                      (make-style))
                      (make-style #:number-format fmt1))
        (check-equal? (compose-styles (make-style)
                                      (make-style))
                      (make-style))))
    
    (test-case "compose-styles: real-world example: editable-percentage-style"
      (define percentage-style          (make-style #:number-format (make-number-format "0%")))
      (define editable-style            (make-style #:locked? #f))
      (define editable-percentage-style (compose-styles percentage-style editable-style))
      (check-equal? (style-number-format percentage-style)
                    (style-number-format editable-percentage-style))
      (check-equal? (style-locked? editable-style)
                    (style-locked? editable-percentage-style)))))

; Provide statements -----------------------------

(provide struct-tests)