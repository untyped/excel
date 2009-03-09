#lang scheme/base

(require "test-base.ss")

(require "struct-style.ss"
         (prefix-in internal: (only-in "struct-style-internal.ss"
                                       make-font
                                       make-style)))

(require/expose "struct-style.ss"
  (compose-normal compose-boolean))

; Helpers ----------------------------------------

;  (any any -> any)
;  any
;  any
;  any
; ->
;  void
(define-check (check-compose compose empty non-empty1 non-empty2)
  (check-equal? (compose empty      non-empty1) non-empty1)
  (check-equal? (compose non-empty1 empty)      non-empty1)
  (check-equal? (compose non-empty1 non-empty2) non-empty2)
  (check-equal? (compose non-empty2 non-empty1) non-empty1))

; (any -> boolean) ((U boolean void) -> any) -> void
(define-check (check-boolean-accessor accessor constructor)
  (check-true  (accessor (constructor #t)))
  (check-false (accessor (constructor #f)))
  (check-false (accessor (constructor (void)))))

; Tests ------------------------------------------

(define struct-style-tests
  (test-suite "struct-style.ss"
    
    (test-case "make-rgba-color"
      (check-not-exn (cut make-rgba-color 0 0 0 0))
      (check-not-exn (cut make-rgba-color 1 1 1 1))
      (check-exn exn:fail:contract? (cut make-rgba-color 1.1 1.1 1.1 1.1))
      (check-not-exn (cut make-rgba-color 1/2 1/2 1/2 1/2)))
    
    (test-case "make-rgb-color"
      (check-not-exn (cut make-rgb-color 0 0 0))
      (check-not-exn (cut make-rgb-color 1 1 1))
      (check-exn exn:fail:contract? (cut make-rgb-color 1.1 1.1 1.1))
      (check-not-exn (cut make-rgb-color 1/2 1/2 1/2))
      (check-equal? (make-rgb-color .2 .4 .6)
                    (make-rgba-color .2 .4 .6 1)))
    
    (test-equal? "rgb"  (rgb  .2 .4 .6)    (make-rgb-color  .2 .4 .6))
    (test-equal? "rgba" (rgba .2 .4 .6 .8) (make-rgba-color .2 .4 .6 .8))
    
    (test-case "rgba-color-hex"
      (check-equal? (rgba-color-hex (make-rgba-color 0    0   0    0)) "00000000")
      (check-equal? (rgba-color-hex (make-rgba-color 1    1   1    1)) "FFFFFFFF")
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
      (check-boolean-accessor font-bold?        (cut make-font #:bold?        <>))
      (check-boolean-accessor font-italic?      (cut make-font #:italic?      <>))
      (check-boolean-accessor font-underline?   (cut make-font #:underline?   <>))
      (check-boolean-accessor font-outline?     (cut make-font #:outline?     <>))
      (check-boolean-accessor font-shadow?      (cut make-font #:shadow?      <>))
      (check-boolean-accessor font-strike?      (cut make-font #:strike?      <>))
      (check-boolean-accessor font-superscript? (cut make-font #:superscript? <>))
      (check-boolean-accessor font-subscript?   (cut make-font #:subscript?   <>)))
    
    (test-case "compose-fonts"
      (let ([check-compose (cut check-compose compose-fonts empty-font <> <>)]
            [red           (make-rgba-color 1 0 0 1)]
            [green         (make-rgba-color 0 1 0 1)])
        (check-compose (make-font #:name         "Arial") (make-font #:name         "Helvetica"))
        (check-compose (make-font #:size         10)      (make-font #:size         12))
        (check-compose (make-font #:color        red)     (make-font #:color        green))
        (check-compose (make-font #:bold?        #t)      (make-font #:bold?        #f))
        (check-compose (make-font #:italic?      #t)      (make-font #:italic?      #f))
        (check-compose (make-font #:underline?   #t)      (make-font #:underline?   #f))
        (check-compose (make-font #:outline?     #t)      (make-font #:outline?     #f))
        (check-compose (make-font #:shadow?      #t)      (make-font #:shadow?      #f))
        (check-compose (make-font #:strike?      #t)      (make-font #:strike?      #f))
        (check-compose (make-font #:superscript? #t)      (make-font #:superscript? #f))
        (check-compose (make-font #:subscript?   #t)      (make-font #:subscript?   #f))))
    
    (test-equal? "make-solid-fill"
      (make-solid-fill   (make-rgba-color 1 1 1 1))
      (make-pattern-fill (make-rgba-color 1 1 1 1)
                         (make-rgba-color 1 1 1 1)
                         (pattern-type solid)))
    
    (test-case "empty-fill?"
      (check-true  (empty-fill? empty-fill))
      (check-false (empty-fill? (make-linear-gradient-fill
                                 0
                                 (list (make-gradient-stop 0 (make-rgba-color 0 0 0 0))
                                       (make-gradient-stop 1 (make-rgba-color 1 1 1 1))))))
      (check-false (empty-fill? (make-path-gradient-fill
                                 .25 .25 .25 .25
                                 (list (make-gradient-stop 0 (make-rgba-color 0 0 0 0))
                                       (make-gradient-stop 1 (make-rgba-color 1 1 1 1))))))
      (check-false (empty-fill? (make-pattern-fill (make-rgba-color 0 0 0 0)
                                                   (make-rgba-color 1 1 1 1)
                                                   (pattern-type dark-gray)))))
    
    (test-case "compose-fills"
      (let ([check-compose (cut check-compose compose-fills empty-fill <> <>)])
        (check-compose (make-solid-fill   (make-rgba-color 0 0 0 0))
                       (make-solid-fill   (make-rgba-color 0 0 0 1)))
        (check-compose (make-pattern-fill (make-rgba-color 0 0 1 0)
                                          (make-rgba-color 0 0 1 1)
                                          (pattern-type dark-down))
                       (make-pattern-fill (make-rgba-color 0 1 0 0)
                                          (make-rgba-color 0 1 0 1)
                                          (pattern-type dark-gray)))
        (check-compose (make-linear-gradient-fill
                        90
                        (list (make-gradient-stop 0 (make-rgba-color 0 1 1 0))
                              (make-gradient-stop 1 (make-rgba-color 0 1 1 1))))
                       (make-linear-gradient-fill
                        180
                        (list (make-gradient-stop 0 (make-rgba-color 1 0 0 0))
                              (make-gradient-stop 1 (make-rgba-color 1 0 0 1)))))
        (check-compose (make-path-gradient-fill
                        .1 .2 .3 .4
                        (list (make-gradient-stop 0 (make-rgba-color 1 0 1 0))
                              (make-gradient-stop 1 (make-rgba-color 1 0 1 1))))
                       (make-path-gradient-fill
                        .5 .6 .7 .8
                        (list (make-gradient-stop 0 (make-rgba-color 1 1 0 0))
                              (make-gradient-stop 1 (make-rgba-color 1 1 0 1)))))))
    
    (test-case "empty-border?"
      (check-true   (empty-border? (make-border)))
      (check-true   (empty-border? empty-border))
      (check-false  (empty-border? (make-border #:top            (make-line))))
      (check-false  (empty-border? (make-border #:right          (make-line))))
      (check-false  (empty-border? (make-border #:bottom         (make-line))))
      (check-false  (empty-border? (make-border #:left           (make-line))))
      (check-false  (empty-border? (make-border #:horizontal     (make-line))))
      (check-false  (empty-border? (make-border #:vertical       (make-line))))
      (check-false  (empty-border? (make-border #:diagonal       (make-line))))
      (check-false  (empty-border? (make-border #:outline?       #f)))
      (check-false  (empty-border? (make-border #:diagonal-up?   #f)))
      (check-false  (empty-border? (make-border #:diagonal-down? #f))))
    
    (test-case "border-outline?"
      (check-boolean-accessor border-outline?       (cut make-border #:outline?       <>))
      (check-boolean-accessor border-diagonal-down? (cut make-border #:diagonal-down? <>))
      (check-boolean-accessor border-diagonal-up?   (cut make-border #:diagonal-up?   <>)))
    
    (test-case "compose-borders"
      (let ([check-compose (cut check-compose compose-borders empty-border <> <>)]
            [line1         (make-line (border-style thin) (rgb 1 0 0))]
            [line2         (make-line (border-style thin) (rgb 0 0 1))])
        (check-compose (make-border #:top            line1) (make-border #:top            line2))
        (check-compose (make-border #:right          line1) (make-border #:right          line2))
        (check-compose (make-border #:bottom         line1) (make-border #:bottom         line2))
        (check-compose (make-border #:left           line1) (make-border #:left           line2))
        (check-compose (make-border #:horizontal     line1) (make-border #:horizontal     line2))
        (check-compose (make-border #:vertical       line1) (make-border #:vertical       line2))
        (check-compose (make-border #:outline?       #t)    (make-border #:outline?       #f))
        (check-compose (make-border #:diagonal-down? #t)    (make-border #:diagonal-down? #f))
        (check-compose (make-border #:diagonal-up?   #t)    (make-border #:diagonal-up?   #f))
        ; Special rules for border thickness:
        (check-equal? (compose-borders (make-border #:top (make-line (border-style thin)))
                                       (make-border #:top (make-line (border-style thick))))
                      (make-border #:top (make-line (border-style thick))))
        (check-equal? (compose-borders (make-border #:top (make-line (border-style thick)))
                                       (make-border #:top (make-line (border-style thin))))
                      (make-border #:top (make-line (border-style thick))))))
    
    (test-case "border-style and border-style?"
      (check-true (border-style? (border-style none)))
      (check-true (border-style? (border-style hair)))
      (check-true (border-style? (border-style thin)))
      (check-true (border-style? (border-style medium)))
      (check-true (border-style? (border-style thick)))
      (check-true (border-style? (border-style double)))
      (check-true (border-style? (border-style dashed)))
      (check-true (border-style? (border-style dotted)))
      (check-true (border-style? (border-style dash-dot)))
      (check-true (border-style? (border-style dash-dot-dot)))
      (check-true (border-style? (border-style medium-dash-dot)))
      (check-true (border-style? (border-style medium-dash-dot-dot)))
      (check-true (border-style? (border-style medium-dashed)))
      (check-true (border-style? (border-style slant-dash-dot))))
    
    (test-case "empty-alignment?"
      (check-true  (empty-alignment? (make-alignment)))
      (check-true  (empty-alignment? empty-alignment))
      (check-false (empty-alignment? (make-alignment #:horizontal         (horizontal-alignment general))))
      (check-false (empty-alignment? (make-alignment #:vertical           (vertical-alignment center))))
      (check-false (empty-alignment? (make-alignment #:wrap?              #f)))
      (check-false (empty-alignment? (make-alignment #:shrink?            #f)))
      (check-false (empty-alignment? (make-alignment #:rotation           0)))
      (check-false (empty-alignment? (make-alignment #:justify-last-line? #f)))
      (check-false (empty-alignment? (make-alignment #:indent             0)))
      (check-false (empty-alignment? (make-alignment #:relative-indent    0))))
    
    (test-case "compose-alignments"
      (let ([check-compose (cut check-compose compose-alignments empty-alignment <> <>)])
        (check-compose (make-alignment #:horizontal         (horizontal-alignment general))
                       (make-alignment #:horizontal         (horizontal-alignment left)))
        (check-compose (make-alignment #:vertical           (vertical-alignment top))
                       (make-alignment #:vertical           (vertical-alignment bottom)))
        (check-compose (make-alignment #:wrap?              #t)
                       (make-alignment #:wrap?              #f))
        (check-compose (make-alignment #:shrink?            #t)
                       (make-alignment #:shrink?            #f))
        (check-compose (make-alignment #:rotation           90)
                       (make-alignment #:rotation           180))
        (check-compose (make-alignment #:justify-last-line? #t)
                       (make-alignment #:justify-last-line? #f))
        (check-compose (make-alignment #:indent             1)
                       (make-alignment #:indent             2))
        (check-compose (make-alignment #:relative-indent    1)
                       (make-alignment #:relative-indent    2))))
    
    (test-case "horizontal-alignment and horizontal-alignment?"
      (check-true (horizontal-alignment? (horizontal-alignment general)))
      (check-true (horizontal-alignment? (horizontal-alignment left)))
      (check-true (horizontal-alignment? (horizontal-alignment right)))
      (check-true (horizontal-alignment? (horizontal-alignment center)))
      (check-true (horizontal-alignment? (horizontal-alignment center-continuous)))
      (check-true (horizontal-alignment? (horizontal-alignment distributed)))
      (check-true (horizontal-alignment? (horizontal-alignment justify)))
      (check-true (horizontal-alignment? (horizontal-alignment fill))))
    
    (test-case "vertical-alignment and vertical-alignment?"
      (check-true (vertical-alignment? (vertical-alignment top)))
      (check-true (vertical-alignment? (vertical-alignment bottom)))
      (check-true (vertical-alignment? (vertical-alignment center)))
      (check-true (vertical-alignment? (vertical-alignment distributed)))
      (check-true (vertical-alignment? (vertical-alignment justify))))
    
    (test-case "reading-order and reading-order?"
      (check-true (reading-order? (reading-order context-dependent)))
      (check-true (reading-order? (reading-order left-to-right)))
      (check-true (reading-order? (reading-order right-to-left))))
    
    (test-case "reading-order-code"
      (check-equal? (reading-order-code (reading-order context-dependent)) 0)
      (check-equal? (reading-order-code (reading-order left-to-right))     1)
      (check-equal? (reading-order-code (reading-order right-to-left))     2))
    
    #;(test-case "empty-style?"
        (check-true  (empty-style? (make-style)))
        (check-true  (empty-style? empty-style))
        (check-true  (empty-style? (make-style #:number-format (make-number-format #f))))
        (check-false (empty-style? (make-style #:number-format (make-number-format "0"))))
        (check-true  (empty-style? (make-style #:font (make-font #:name #f))))
        (check-false (empty-style? (make-style #:font (make-font #:name "Dave"))))
        (check-false (empty-style? (make-style #:hidden? #f)))
        (check-false (empty-style? (make-style #:locked? #f))))
    
    (test-case "compile-style"
      ; Pre-compiled styles:
      (check-eq? (compile-style empty-style 0 0) empty-style)
      ; Uncompiled styles:
      (let ([uncompiled (make-uncompiled-style
                         (lambda (x y)
                           (make-compiled-style #:fill (make-solid-fill (rgb x y 1)))))])
        (check-equal? (compile-style uncompiled 0 0)
                      (make-compiled-style #:fill (make-solid-fill (rgb 0 0 1))))
        (check-equal? (compile-style uncompiled 1 0)
                      (make-compiled-style #:fill (make-solid-fill (rgb 1 0 1))))))
    
    (test-case "translate-style"
      ; Compiled styles:
      (check-eq? (compile-style (translate-style empty-style 0 0) 0 0) empty-style)
      ; Uncompiled styles:
      (let ([uncompiled (make-uncompiled-style
                         (lambda (x y)
                           (make-compiled-style #:fill (make-solid-fill (rgb x y 1)))))])
        (check-equal? (compile-style uncompiled 0 0)
                      (make-compiled-style #:fill (make-solid-fill (rgb 0 0 1))))
        (check-equal? (compile-style uncompiled 1 0)
                      (make-compiled-style #:fill (make-solid-fill (rgb 1 0 1))))))
    
    (test-case "compose-styles"
      ; Compiled styles:
      (let ([check-compose (cut check-compose compose-styles empty-style <> <>)])
        (check-compose (make-compiled-style #:fill (make-solid-fill (rgb 1 0 0)))
                       (make-compiled-style #:fill (make-solid-fill (rgb 1 0 0)))))
      ; Uncompiled styles:
      (let* ([style1  (make-uncompiled-style
                       (lambda (x y)
                         (make-compiled-style #:fill (make-solid-fill (rgb x .5 .5)))))]
             [style2  (make-uncompiled-style
                       (lambda (x y)
                         (make-compiled-style #:fill (make-solid-fill (rgb .5 x .5))
                                              #:font (make-font #:size y))))]
             [style12 (compose-styles style1 style2)]
             [style21 (compose-styles style2 style1)])
        (check-equal? (compile-style style12 1 5)
                      (make-compiled-style #:fill (make-solid-fill (rgb .5 1 .5))
                                           #:font (make-font #:size 5)))
        (check-equal? (compile-style style21 1 5)
                      (make-compiled-style #:fill (make-solid-fill (rgb 1 .5 .5))
                                           #:font (make-font #:size 5))))
      ; Mix of compiled and uncompiled styles:
      (let* ([style1  (make-compiled-style #:fill (make-solid-fill (rgb .25 .25 .25)))]
             [style2  (make-uncompiled-style
                       (lambda (x y)
                         (make-compiled-style #:fill (make-solid-fill (rgb .5 x .5))
                                              #:font (make-font #:size y))))]
             [style12 (compose-styles style1 style2)]
             [style21 (compose-styles style2 style1)])
        (check-equal? (compile-style style12 1 5)
                      (make-compiled-style #:fill (make-solid-fill (rgb .5 1 .5))
                                           #:font (make-font #:size 5)))
        (check-equal? (compile-style style21 1 5)
                      (make-compiled-style #:fill (make-solid-fill (rgb .25 .25 .25))
                                           #:font (make-font #:size 5)))))))

; Provide statements -----------------------------

(provide struct-style-tests)