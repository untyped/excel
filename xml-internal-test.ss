#lang scheme/base

(require "test-base.ss")

(require "xml-cache.ss"
         "range.ss"
         "struct.ss"
         ; Need an uncontracted version of range-fold for testing:
         (except-in "xml-internal.ss" range-fold))

(require/expose "xml-internal.ss"
  (range-fold))

; Tests ------------------------------------------

(define xml-internal-tests
  (test-suite "xml-internal.ss"
    
    (test-case "range-fold"
      (let ([accum null])
        (range-fold (hc-append #:style
                               (make-uncompiled-style
                                (lambda (x y)
                                  (make-compiled-style #:fill (make-solid-fill (rgb (/ x 4)
                                                                                    (/ y 4)
                                                                                    0)))))
                               "A"
                               (vc-append #:style
                                          (make-uncompiled-style
                                           (lambda (x y)
                                             (make-compiled-style #:font (make-font #:size y))))
                                          "B"
                                          "C"
                                          "D")
                               "E")
                    empty-style
                    (lambda (x y cell style)
                      (set! accum (cons (list x y cell style) accum)))
                    0
                    0)
        (check-equal? (length accum) 9)
        ; First five entries are from subranges:
        (check-equal?
         (list-ref (reverse accum) 0)
         (list 0 1
               (make-cell "A")
               (make-compiled-style #:fill (make-solid-fill (rgb (/ 0 4) (/ 1 4) 0)))))
        (check-equal?
         (list-ref (reverse accum) 1)
         (list 1 0
               (make-cell "B")
               (make-compiled-style #:fill (make-solid-fill (rgb (/ 1 4) (/ 0 4) 0))
                                    #:font (make-font #:size 0))))
        (check-equal?
         (list-ref (reverse accum) 2)
         (list 1 1
               (make-cell "C")
               (make-compiled-style #:fill (make-solid-fill (rgb (/ 1 4) (/ 1 4) 0))
                                    #:font (make-font #:size 1))))
        (check-equal?
         (list-ref (reverse accum) 3)
         (list 1 2
               (make-cell "D")
               (make-compiled-style #:fill (make-solid-fill (rgb (/ 1 4) (/ 2 4) 0))
                                    #:font (make-font #:size 2))))
        (check-equal?
         (list-ref (reverse accum) 4)
         (list 2 1
               (make-cell "E")
               (make-compiled-style #:fill (make-solid-fill (rgb (/ 2 4) (/ 1 4) 0)))))
        ; Last four entries are from blank spaces:
        (check-equal?
         (list-ref (reverse accum) 5)
         (list 0 0 #f (make-compiled-style #:fill (make-solid-fill (rgb (/ 0 4) (/ 0 4) 0)))))
        (check-equal?
         (list-ref (reverse accum) 6)
         (list 2 0 #f (make-compiled-style #:fill (make-solid-fill (rgb (/ 2 4) (/ 0 4) 0)))))
        (check-equal?
         (list-ref (reverse accum) 7)
         (list 0 2 #f (make-compiled-style #:fill (make-solid-fill (rgb (/ 0 4) (/ 2 4) 0)))))
        (check-equal?
         (list-ref (reverse accum) 8)
         (list 2 2 #f (make-compiled-style #:fill (make-solid-fill (rgb (/ 2 4) (/ 2 4) 0)))))))))

; Provide statements -----------------------------

(provide xml-internal-tests)
