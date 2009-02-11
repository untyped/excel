#lang scheme/base

(require "struct.ss"
         "test-base.ss")

(define struct-tests
  (test-suite "struct.ss"
    
    (test-case "style-empty?"
      (check-true (style-empty? (make-style)))
      (check-false (style-empty? (make-style #:number-format general-number-format))))
    
    (test-case "style-compose"
      (let ([fmt1 (make-number-format #f)]
            [fmt2 (make-number-format "0")])
        (check-equal? (style-compose (make-style #:number-format fmt1)
                                     (make-style #:number-format fmt2))
                      (make-style #:number-format fmt2))
        (check-equal? (style-compose (make-style)
                                     (make-style #:number-format fmt2))
                      (make-style #:number-format fmt2))
        (check-equal? (style-compose (make-style #:number-format fmt1)
                                     (make-style))
                      (make-style #:number-format fmt1))
        (check-equal? (style-compose (make-style)
                                     (make-style))
                      (make-style))))
    
    (test-case "style-compose: real-world example: editable-percentage-style"
      (define percentage-style          (make-style #:number-format (make-number-format "0%")))
      (define editable-style            (make-style #:locked? #f))
      (define editable-percentage-style (style-compose percentage-style editable-style))
      (check-equal? (style-number-format percentage-style)
                    (style-number-format editable-percentage-style))
      (check-equal? (style-locked? editable-style)
                    (style-locked? editable-percentage-style)))))

; Provide statements -----------------------------

(provide struct-tests)