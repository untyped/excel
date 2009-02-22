#!/usr/bin/env mzscheme
#lang scheme

(require "main.ss")

; Helpers ----------------------------------------

; natural natural [style] -> range
(define (make-matrix width height [style empty-style])
  (make-union (for*/list ([x (in-range 0 width)]
                          [y (in-range 0 height)])
                         (make-part (make-cell (format "~a,~a" x y)) x y))
              width height style))

; style
(define heading-style
  (make-compiled-style #:font (make-font #:name  "Trebuchet MS" 
                                         #:size  24
                                         #:color (rgb 1 .5 0)
                                         #:bold? #t)))

; Number formats ---------------------------------

; Fonts ------------------------------------------

; Fills ------------------------------------------

(define fill-sheet
  (make-worksheet
   "Fill"
   (vl-append (make-cell "FILL TESTS" heading-style)
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
                                                        (make-compiled-style #:fill fill))))))))))
              (ht-append (make-cell "TODO: Patterns"))
              (ht-append (make-cell "TODO: Linear gradients"))
              (ht-append (make-cell "TODO: Path gradients")))))

; Borders ----------------------------------------

(define border-sheet
  (make-worksheet
   "Border"
   (vl-append (make-cell "BORDER TESTS" heading-style)
              (t-pad (apply ht-append
                            (make-cell "STYLES")
                            (for/list ([style (in-list border-styles)])
                              (l-pad (make-cell style (make-compiled-style #:border (make-border #:bottom (make-line style))))))))
              (t-pad (ht-append (make-cell "SIDES")
                                (l-pad (make-cell "top"              (make-compiled-style #:border (make-border #:top            (make-line)))))
                                (l-pad (make-cell "right"            (make-compiled-style #:border (make-border #:right          (make-line)))))
                                (l-pad (make-cell "bottom"           (make-compiled-style #:border (make-border #:bottom         (make-line)))))
                                (l-pad (make-cell "left"             (make-compiled-style #:border (make-border #:left           (make-line)))))
                                (l-pad (make-cell "horizontal"       (make-compiled-style #:border (make-border #:horizontal     (make-line)))))
                                (l-pad (make-cell "vertical"         (make-compiled-style #:border (make-border #:vertical       (make-line)))))
                                (l-pad (make-cell "diagonal default" (make-compiled-style #:border (make-border #:diagonal       (make-line)))))
                                (l-pad (make-cell "diagonal down"    (make-compiled-style #:border (make-border #:diagonal       (make-line)
                                                                                                                #:diagonal-down? #t))))
                                (l-pad (make-cell "diagonal up"      (make-compiled-style #:border (make-border #:diagonal       (make-line)
                                                                                                                #:diagonal-up?   #t))))
                                (l-pad (make-cell "diagonal both"    (make-compiled-style #:border (make-border #:diagonal       (make-line)
                                                                                                                #:diagonal-down? #t
                                                                                                                #:diagonal-up?   #t))))
                                (l-pad (make-cell "trbl"             (make-compiled-style #:border (make-border #:top            (make-line)
                                                                                                                #:right          (make-line)
                                                                                                                #:bottom         (make-line)
                                                                                                                #:left           (make-line)))))
                                (l-pad (make-cell "everything"       (make-compiled-style #:border (make-border #:top            (make-line)
                                                                                                                #:right          (make-line)
                                                                                                                #:bottom         (make-line)
                                                                                                                #:left           (make-line)
                                                                                                                #:horizontal     (make-line)
                                                                                                                #:vertical       (make-line)
                                                                                                                #:diagonal       (make-line)
                                                                                                                #:diagonal-down? #t
                                                                                                                #:diagonal-up?   #t))))))
              (t-pad (ht-append (make-cell "REGIONS")
                                (l-pad (vl-append "Default"
                                                  (outline-range (make-matrix 3 3))))
                                (l-pad (vl-append "Thick"
                                                  (outline-range (make-matrix 3 3)
                                                                 (border-style thick))))
                                (l-pad (vl-append "Thick red"
                                                  (outline-range (make-matrix 3 3)
                                                                 (border-style thick)
                                                                 (rgb 1 0 0)))))))))

(define compound-sheet
  (make-worksheet
   "Compound"
   (vl-append (make-cell "COMPOUND TESTS" heading-style)
              (l-pad (t-pad (vl-append "Tabulate"
                                       (t-pad (tabulate (make-matrix 10 20)
                                                        (make-matrix 10 1)
                                                        (make-matrix 1 20)))))))))

; Alignment --------------------------------------

(define alignment-sheet
  (make-worksheet
   "Alignment"
   (vl-append (make-cell "ALIGNMENT TESTS" heading-style)
              (ht-append (make-cell "HORIZONTAL")
                         (make-cell "General"           (make-compiled-style #:alignment (make-alignment #:horizontal (horizontal-alignment general))))
                         (make-cell "Left"              (make-compiled-style #:alignment (make-alignment #:horizontal (horizontal-alignment left))))
                         (make-cell "Right"             (make-compiled-style #:alignment (make-alignment #:horizontal (horizontal-alignment right))))
                         (make-cell "Center"            (make-compiled-style #:alignment (make-alignment #:horizontal (horizontal-alignment center))))
                         (make-cell "Center continuous" (make-compiled-style #:alignment (make-alignment #:horizontal (horizontal-alignment center-continuous))))
                         (make-cell "Distributed"       (make-compiled-style #:alignment (make-alignment #:horizontal (horizontal-alignment distributed))))
                         (make-cell "Justify"           (make-compiled-style #:alignment (make-alignment #:horizontal (horizontal-alignment justify))))
                         (make-cell "Fill"              (make-compiled-style #:alignment (make-alignment #:horizontal (horizontal-alignment fill)))))
              (ht-append (make-cell "VERTICAL")
                         (make-cell "Top"               (make-compiled-style #:alignment (make-alignment #:vertical (vertical-alignment top))))
                         (make-cell "Bottom"            (make-compiled-style #:alignment (make-alignment #:vertical (vertical-alignment bottom))))
                         (make-cell "Center"            (make-compiled-style #:alignment (make-alignment #:vertical (vertical-alignment center))))
                         (make-cell "Distributed"       (make-compiled-style #:alignment (make-alignment #:vertical (vertical-alignment distributed))))
                         (make-cell "Justify"           (make-compiled-style #:alignment (make-alignment #:vertical (vertical-alignment justify)))))
              (ht-append (make-cell "MIXED")
                         (apply hc-append (for/list ([h (in-list horizontal-alignments)])
                                            (apply vc-append (for/list ([v (in-list vertical-alignments)])
                                                               (let* ([align (make-alignment #:horizontal h #:vertical v)]
                                                                      [style (make-compiled-style #:alignment align)])
                                                                 (make-cell (format "~a ~a" h v) style)))))))
              (ht-append (make-cell "WRAP/SHRINK")
                         (make-cell "Wrapped: this is a very long cell value." (make-compiled-style #:alignment (make-alignment #:wrap? #t)))
                         (make-cell "Shrunk: this is a very long cell value."  (make-compiled-style #:alignment (make-alignment #:shrink? #t))))
              (apply ht-append
                     (make-cell "ANGLE")
                     (for/list ([a (in-range 0 181 45)])
                       (make-cell (format "Angle: ~a" a) (make-compiled-style #:alignment (make-alignment #:rotation a)))))
              (apply ht-append
                     (make-cell "READING ORDER")
                     (for/list ([o (in-list reading-orders)])
                       (make-cell (format "Order: ~a" o) (make-compiled-style #:alignment (make-alignment #:reading-order o))))))))

; Conditional formatting -------------------------

(define conditional-format-sheet
  (make-worksheet
   "Conditional formatting"
   (vl-append (make-cell "CONDITIONAL FORMATTING" heading-style)
              (t-pad (ht-append "Constants"
                                (let ([cfs (list (make-conditional-format (condition-type cell-is)
                                                                          (fx (* 1 1))
                                                                          (make-compiled-style #:fill (make-solid-fill (rgb 1 .5 .5)))
                                                                          1)
                                                 (make-conditional-format (condition-type cell-is)
                                                                          (fx (* 2 1))
                                                                          (make-compiled-style #:font (make-font #:bold? #t))
                                                                          2)
                                                 (make-conditional-format (condition-type cell-is)
                                                                          (fx (* 3 1))
                                                                          (make-compiled-style #:border (make-border #:left (make-line) #:right (make-line)))
                                                                          3))])
                                  (l-pad (vl-append (make-cell 1 empty-style cfs)
                                                    (make-cell 2 empty-style cfs)
                                                    (make-cell 3 empty-style cfs)))))))))

; Workbook ---------------------------------------

(write-workbook (make-workbook (list fill-sheet 
                                     border-sheet
                                     alignment-sheet
                                     compound-sheet
                                     conditional-format-sheet))
                (build-path (current-directory) "swatch.xlsx")
                #:exists 'replace)