#lang scheme/base

(require "struct-style.ss"
         "test-base.ss"
         (prefix-in internal: (only-in "struct-style-internal.ss"
                                       make-font
                                       make-style)))

(require/expose "struct-style.ss"
  (compose-normal compose-boolean))

(define struct-style-tests
  (test-suite "struct-style.ss"
    
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
    
    (test-case "font-compose"
      (check-equal? (font-compose #f #f) (make-font))
      (check-equal? (font-compose #f (make-font)) (make-font))
      (check-equal? (font-compose (make-font) #f) (make-font))
      (check-equal? (font-compose (make-font #:bold? #t #:underline? #t)
                                  (make-font #:bold? #f #:italic? #f))
                    (make-font #:bold? #f #:italic? #f #:underline? #t)))
    
    (test-case "style-empty?"
      (check-true  (style-empty? (make-style)))
      (check-true  (style-empty? (make-style #:number-format (make-number-format #f))))
      (check-false (style-empty? (make-style #:number-format (make-number-format "0"))))
      (check-true  (style-empty? (make-style #:font (make-font #:name #f))))
      (check-false (style-empty? (make-style #:font (make-font #:name "Dave"))))
      (check-false (style-empty? (make-style #:hidden? #f)))
      (check-false (style-empty? (make-style #:locked? #f))))
    
    (test-case "style-compose"
      (let ([fmt1  (make-number-format #f)]
            [fmt2  (make-number-format "0")]
            [font1 (make-font #:name "Dave")]
            [font2 (make-font #:name "David")])
        (check-equal? (style-compose (make-style #:number-format fmt1
                                                 #:font          font1)
                                     (make-style #:number-format fmt2
                                                 #:hidden?       #f))
                      (make-style #:number-format fmt2
                                  #:font font1
                                  #:hidden? #f))
        (check-equal? (style-compose (make-style) (make-style #:number-format fmt2))
                      (make-style #:number-format fmt2))
        (check-equal? (style-compose (make-style #:number-format fmt1) (make-style))
                      (make-style #:number-format fmt1))
        (check-equal? (style-compose (make-style) (make-style)) (make-style))))))

; Provide statements -----------------------------

(provide struct-style-tests)