#lang scheme/base

(require "base.ss")

(require "struct.ss")

; -> (-> natural)
(define (make-counter initial)
  (let ([counter initial])
    (lambda ()
      (begin0 counter
              (set! counter (add1 counter))))))

; range style (natural natural (U cell #f) compiled-style -> void) [natural] [natural] -> void
;
; Computes the compiled style in each (x,y) position in the range
; and calls accumulate! with the style and the cell in that square
; (if any).
(define (range-fold range base-style accumulate! [x0 0] [y0 0])
  (let ([style (compose-styles base-style (range-style range))])

    (if (cell? range)
        
        ; Cell:
        (accumulate! x0 y0 range (compile-style style 0 0))
        
        ; Union:
        (let ([parts (union-parts range)])
          
          ; Recurse to children:
          (for ([part (in-list parts)])
            (let ([dx (part-dx part)]
                  [dy (part-dy part)])
              (range-fold (part-range part)
                          (translate-style style dx dy)
                          accumulate!
                          (+ x0 dx)
                          (+ y0 dy))))
          
          ; Fill in cells not covered by children:
          (for* ([y (in-range (range-height range))]
                 [x (in-range (range-width range))])
                (unless (ormap (cut part-contains? <> x y) parts)
                  (accumulate! (+ x x0) (+ y y0) #f (compile-style style x y))))))))

; Provide statements -----------------------------

(provide/contract
 [make-counter (-> natural-number/c (-> natural-number/c))]
 [range-fold   (->* (range? 
                     style?
                     (-> natural-number/c
                         natural-number/c
                         (or/c cell? #f)
                         compiled-style?
                         void?))
                    (natural-number/c
                     natural-number/c)
                   void?)])
