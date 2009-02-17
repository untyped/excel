#lang scheme/base

(require "test-base.ss")

(require "struct-style.ss"
         (prefix-in internal: (only-in "struct-style-internal.ss"
                                       make-font
                                       make-style)))

(require/expose "struct-style.ss"
  (compose-normal compose-boolean))

(define struct-style-tests
  (test-suite "struct-style.ss"
    
    (test-case "make-rgba-color"
      (check-not-exn (cut make-rgba-color 0 0 0 0))
      (check-not-exn (cut make-rgba-color 1 1 1 1))
      (check-exn exn:fail:contract? (cut make-rgba-color 1.1 1.1 1.1 1.1))
      (check-not-exn (cut make-rgba-color 1/2 1/2 1/2 1/2)))
    
    (test-case "rgba-color-hex"
      (check-equal? (rgba-color-hex (make-rgba-color 0   0   0   0))   "00000000")
      (check-equal? (rgba-color-hex (make-rgba-color 1   1   1   1))   "FFFFFFFF")
      (check-equal? (rgba-color-hex (make-rgba-color 0.25 0.5 0.75 1)) "FF3F7FBF")
      (check-equal? (rgba-color-hex (make-rgba-color 1/4 2/4 3/4 4/4)) "FF3F7FBF"))
    
    (test-case "compose-normal"
      (check-equal? (compose-normal (lambda (x) x) 12 #f) 12)
      (check-equal? (compose-normal (lambda (x) x) #f 34) 34)
      (check-equal? (compose-normal (lambda (x) x) 12 34) 34)
      (check-equal? (compose-normal (lambda (x) x) #f #f) #f))
    
    (test-case "compose-boolean"
      (check-equal? (compose-boolean (lambda (x) x) #t #f) #f)
      (check-equal? (compose-boolean (lambda (x) x) #f #t) #t)
      (check-equal? (compose-boolean (lambda (x) x) #t #t) #t)
      (check-equal? (compose-boolean (lambda (x) x) #f #f) #f)
      (check-equal? (compose-boolean (lambda (x) x) #t (void)) #t)
      (check-equal? (compose-boolean (lambda (x) x) #f (void)) #f)
      (check-equal? (compose-boolean (lambda (x) x) (void) #t) #t)
      (check-equal? (compose-boolean (lambda (x) x) (void) #f) #f))
    
    (test-case "make-font"
      (check-equal? (make-font)
                    (internal:make-font #f #f #f (void) (void) (void) (void) (void) (void) (void) (void)))
      (check-equal? (make-font #:name "Dave")
                    (internal:make-font "Dave" #f #f (void) (void) (void) (void) (void) (void) (void) (void)))
      (check-equal? (make-font #:bold? #t #:underline? #t)
                    (internal:make-font #f #f #f #t (void) #t (void) (void) (void) (void) (void))))
    
    (test-case "font-{bold|italic|etc}?"
      (check-false (font-bold?      (make-font)))
      (check-false (font-underline? (make-font)))
      (check-true  (font-bold?      (make-font #:bold? #t)))
      (check-false (font-underline? (make-font #:bold? #t)))
      (check-false (font-bold?      (make-font #:bold? #f)))
      (check-false (font-underline? (make-font #:bold? #f)))
      (check-false (font-bold?      (make-font #:underline? #t)))
      (check-true  (font-underline? (make-font #:underline? #t))))
    
    (test-case "compose-fonts"
      (check-equal? (compose-fonts (make-font #:bold? #t #:underline? #t)
                                   (make-font #:bold? #f #:italic? #f))
                    (make-font #:bold? #f #:italic? #f #:underline? #t)))
    
    (test-equal? "make-solid-fill"
      (make-solid-fill (make-rgba-color 1 1 1 1))
      (make-pattern-fill (make-rgba-color 1 1 1 1)
                         (make-rgba-color 1 1 1 1)
                         (pattern-type solid)))
    
    (test-case "empty-fill?"
      (check-true  (empty-fill? (make-empty-fill)))
      (check-true  (empty-fill? empty-fill))
      (check-false (empty-fill? (make-linear-gradient-fill 0 (list (make-gradient-stop 0 (make-rgba-color 0 0 0 0))
                                                                   (make-gradient-stop 1 (make-rgba-color 1 1 1 1))))))
      (check-false (empty-fill? (make-path-gradient-fill .25 .25 .25 .25
                                                         (list (make-gradient-stop 0 (make-rgba-color 0 0 0 0))
                                                               (make-gradient-stop 1 (make-rgba-color 1 1 1 1))))))
      (check-false (empty-fill? (make-pattern-fill (make-rgba-color 0 0 0 0)
                                                   (make-rgba-color 1 1 1 1)
                                                   (pattern-type dark-gray)))))
    
    (test-case "compose-fills"
      (check-equal? (compose-fills empty-fill
                                   (make-solid-fill (make-rgba-color 1 1 1 1)))
                    (make-solid-fill (make-rgba-color 1 1 1 1)))
      (check-equal? (compose-fills (make-solid-fill (make-rgba-color 0 0 0 0))
                                   empty-fill)
                    (make-solid-fill (make-rgba-color 0 0 0 0)))
      (check-equal? (compose-fills (make-solid-fill (make-rgba-color 0 0 0 0))
                                   (make-solid-fill (make-rgba-color 1 1 1 1)))
                    (make-solid-fill (make-rgba-color 1 1 1 1))))
    
    (test-case "empty-style?"
      (check-true  (empty-style? (make-style)))
      (check-true  (empty-style? (make-style #:number-format (make-number-format #f))))
      (check-false (empty-style? (make-style #:number-format (make-number-format "0"))))
      (check-true  (empty-style? (make-style #:font (make-font #:name #f))))
      (check-false (empty-style? (make-style #:font (make-font #:name "Dave"))))
      (check-false (empty-style? (make-style #:hidden? #f)))
      (check-false (empty-style? (make-style #:locked? #f))))
    
    (test-case "compose-styles"
      (let ([fmt1  (make-number-format #f)]
            [fmt2  (make-number-format "0")]
            [font1 (make-font #:name "Dave")]
            [font2 (make-font #:name "David")])
        (check-equal? (compose-styles (make-style #:number-format fmt1
                                                  #:font          font1)
                                      (make-style #:number-format fmt2
                                                  #:hidden?       #f))
                      (make-style #:number-format fmt2
                                  #:font font1
                                  #:hidden? #f))
        (check-equal? (compose-styles (make-style) (make-style #:number-format fmt2))
                      (make-style #:number-format fmt2))
        (check-equal? (compose-styles (make-style #:number-format fmt1) (make-style))
                      (make-style #:number-format fmt1))
        (check-equal? (compose-styles (make-style) (make-style)) (make-style))))))

; Provide statements -----------------------------

(provide struct-style-tests)