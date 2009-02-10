#lang scheme/base

(require "struct.ss"
         "test-base.ss")

(define struct-tests
  (test-suite "struct.ss"
    
    (test-case "style-empty?"
      (check-true (style-empty? (make-style #f)))
      (check-false (style-empty? (make-style (make-number-format #f)))))
    
    (test-case "style-compose"
      (let ([fmt1 (make-number-format #f)]
            [fmt2 (make-number-format "0")])
        (check-equal? (style-compose (make-style fmt1)
                                     (make-style fmt2))
                      (make-style fmt2))
        (check-equal? (style-compose (make-style #f)
                                     (make-style fmt2))
                      (make-style fmt2))
        (check-equal? (style-compose (make-style fmt1)
                                     (make-style #f))
                      (make-style fmt1))
        (check-equal? (style-compose (make-style #f)
                                     (make-style #f))
                      (make-style #f))))))

; Provide statements -----------------------------

(provide struct-tests)