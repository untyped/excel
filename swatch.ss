#!/usr/bin/env mzscheme
#lang scheme

(require "main.ss")

; Helpers ----------------------------------------

; natural natural [style] -> range
(define (make-matrix width height [style empty-style])
  (make-union (for*/list ([x (in-range 0 width)] [y (in-range 0 width)])
                         (make-part (make-cell (format "~a,~a" x y)) x y))
              width height style))

; Number formats ---------------------------------

; Fonts ------------------------------------------

; Fills ------------------------------------------

(define fill-sheet
  (make-worksheet
   "Fill"
   (vl-append (make-cell "FILL TESTS")
              (apply ht-append
                     (make-cell "SOLID")
                     (let ([step 0.1])
                       (for/list ([r (in-range 0 1.01 step)])
                         (apply vc-append
                                (for/list ([g (in-range 0 1.01 step)])
                                  (apply hc-append
                                         (for/list ([b (in-range 0 1.01 step)])
                                           (let* ([color (make-rgba-color r g b 1)]
                                                  [fill  (make-solid-fill color)])
                                             (make-cell (rgba-color-hex color)
                                                        (make-style #:fill fill))))))))))
              (ht-append (make-cell "TODO: Patterns"))
              (ht-append (make-cell "TODO: Linear gradients"))
              (ht-append (make-cell "TODO: Path gradients")))))

; Borders ----------------------------------------

(define border-sheet
  (make-worksheet
   "Border"
   (vl-append (make-cell "BORDER TESTS")
              (t-pad (apply ht-append
                            (make-cell "STYLES")
                            (for/list ([style (in-list border-styles)])
                              (l-pad (make-cell style (make-style #:border (make-border #:bottom (make-line style))))))))
              (t-pad (ht-append (make-cell "SIDES")
                                (l-pad (make-cell "top"              (make-style #:border (make-border #:top            (make-line)))))
                                (l-pad (make-cell "right"            (make-style #:border (make-border #:right          (make-line)))))
                                (l-pad (make-cell "bottom"           (make-style #:border (make-border #:bottom         (make-line)))))
                                (l-pad (make-cell "left"             (make-style #:border (make-border #:left           (make-line)))))
                                (l-pad (make-cell "horizontal"       (make-style #:border (make-border #:horizontal     (make-line)))))
                                (l-pad (make-cell "vertical"         (make-style #:border (make-border #:vertical       (make-line)))))
                                (l-pad (make-cell "diagonal default" (make-style #:border (make-border #:diagonal       (make-line)))))
                                (l-pad (make-cell "diagonal down"    (make-style #:border (make-border #:diagonal       (make-line)
                                                                                                       #:diagonal-down? #t))))
                                (l-pad (make-cell "diagonal up"      (make-style #:border (make-border #:diagonal       (make-line)
                                                                                                       #:diagonal-up?   #t))))
                                (l-pad (make-cell "diagonal both"    (make-style #:border (make-border #:diagonal       (make-line)
                                                                                                       #:diagonal-down? #t
                                                                                                       #:diagonal-up?   #t))))
                                (l-pad (make-cell "trbl"             (make-style #:border (make-border #:top            (make-line)
                                                                                                       #:right          (make-line)
                                                                                                       #:bottom         (make-line)
                                                                                                       #:left           (make-line)))))
                                (l-pad (make-cell "everything"       (make-style #:border (make-border #:top            (make-line)
                                                                                                       #:right          (make-line)
                                                                                                       #:bottom         (make-line)
                                                                                                       #:left           (make-line)
                                                                                                       #:horizontal     (make-line)
                                                                                                       #:vertical       (make-line)
                                                                                                       #:diagonal       (make-line)
                                                                                                       #:diagonal-down? #t
                                                                                                       #:diagonal-up?   #t))))))
              (t-pad (ht-append (make-cell "REGIONS")
                                (l-pad (vl-append (make-cell "top and right")
                                                  (make-matrix 3 3 (make-style #:border (make-border #:top   (make-line)
                                                                                                     #:right (make-line))))))
                                (l-pad (vl-append (make-cell "bottom and right")
                                                  (make-matrix 3 3 (make-style #:border (make-border #:bottom (make-line)
                                                                                                     #:right  (make-line))))))
                                (l-pad (vl-append (make-cell "bottom and left")
                                                  (make-matrix 3 3 (make-style #:border (make-border #:bottom (make-line)
                                                                                                     #:left   (make-line))))))
                                (l-pad (vl-append (make-cell "top and left")
                                                  (make-matrix 3 3 (make-style #:border (make-border #:top   (make-line)
                                                                                                     #:left  (make-line))))))
                                (l-pad (vl-append (make-cell "horizontal")
                                                  (make-matrix 3 3 (make-style #:border (make-border #:horizontal (make-line)
                                                                                                     #:outline?   #t)))))
                                (l-pad (vl-append (make-cell "vertical")
                                                  (make-matrix 3 3 (make-style #:border (make-border #:vertical   (make-line)
                                                                                                     #:outline?   #t))))))))))

; Alignment --------------------------------------

(define alignment-sheet
  (make-worksheet
   "Alignment"
   (vl-append (make-cell "ALIGNMENT TESTS")
              (ht-append (make-cell "HORIZONTAL")
                         (make-cell "General"           (make-style #:alignment (make-alignment #:horizontal (horizontal-alignment general))))
                         (make-cell "Left"              (make-style #:alignment (make-alignment #:horizontal (horizontal-alignment left))))
                         (make-cell "Right"             (make-style #:alignment (make-alignment #:horizontal (horizontal-alignment right))))
                         (make-cell "Center"            (make-style #:alignment (make-alignment #:horizontal (horizontal-alignment center))))
                         (make-cell "Center continuous" (make-style #:alignment (make-alignment #:horizontal (horizontal-alignment center-continuous))))
                         (make-cell "Distributed"       (make-style #:alignment (make-alignment #:horizontal (horizontal-alignment distributed))))
                         (make-cell "Justify"           (make-style #:alignment (make-alignment #:horizontal (horizontal-alignment justify))))
                         (make-cell "Fill"              (make-style #:alignment (make-alignment #:horizontal (horizontal-alignment fill)))))
              (ht-append (make-cell "VERTICAL")
                         (make-cell "Top"               (make-style #:alignment (make-alignment #:vertical (vertical-alignment top))))
                         (make-cell "Bottom"            (make-style #:alignment (make-alignment #:vertical (vertical-alignment bottom))))
                         (make-cell "Center"            (make-style #:alignment (make-alignment #:vertical (vertical-alignment center))))
                         (make-cell "Distributed"       (make-style #:alignment (make-alignment #:vertical (vertical-alignment distributed))))
                         (make-cell "Justify"           (make-style #:alignment (make-alignment #:vertical (vertical-alignment justify)))))
              (ht-append (make-cell "MIXED")
                         (apply hc-append (for/list ([h (in-list horizontal-alignments)])
                                            (apply vc-append (for/list ([v (in-list vertical-alignments)])
                                                               (let* ([align (make-alignment #:horizontal h #:vertical v)]
                                                                      [style (make-style #:alignment align)])
                                                                 (make-cell (format "~a ~a" h v) style)))))))
              (ht-append (make-cell "WRAP/SHRINK")
                         (make-cell "Wrapped: this is a very long cell value." (make-style #:alignment (make-alignment #:wrap? #t)))
                         (make-cell "Shrunk: this is a very long cell value."  (make-style #:alignment (make-alignment #:shrink? #t))))
              (apply ht-append
                     (make-cell "ANGLE")
                     (for/list ([a (in-range 0 181 45)])
                       (make-cell (format "Angle: ~a" a) (make-style #:alignment (make-alignment #:rotation a)))))
              (apply ht-append
                     (make-cell "READING ORDER")
                     (for/list ([o (in-list reading-orders)])
                       (make-cell (format "Order: ~a" o) (make-style #:alignment (make-alignment #:reading-order o))))))))

; Workbook ---------------------------------------

(write-workbook (make-workbook (list fill-sheet 
                                     border-sheet
                                     alignment-sheet))
                (build-path (current-directory) "swatch.xlsx")
                #:exists 'replace)