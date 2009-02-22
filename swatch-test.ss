#lang scheme/base

(require "test-base.ss")

(define swatch-tests
  (test-suite "swatch"
    
    (test-case "swatch created without exceptions"
      (check-not-exn
        (lambda ()
          (dynamic-require "swatch.ss" #f)))
      (printf "Swatch workbook created in ~a.~n" (build-path (current-directory) "swatch.xlsx"))
      (printf "Review this workbook to complete the tests.~n"))))

; Provide statements -----------------------------

(provide swatch-tests)