#lang scheme/base

(require "test-base.ss")

(require "range.ss"
         "struct.ss"
         "xml-cache.ss"
         "xml-internal.ss"
         "xml-style.ss")

(require/expose "xml-style.ss"
  (number-format-xml font-xml fill-xml border-xml style-xml))

; xml xml -> void
(define (check-xml xml1 xml2)
  (check-equal? (xml->string xml1)
                (xml->string xml2)))

; Tests ------------------------------------------

(define xml-style-tests
  (test-suite "xml-style.ss"
    
    (test-case "number-format-xml"
      (check-xml (number-format-xml empty-number-format 123)
                 (xml (numFmt (@ [numFmtId 123] [formatCode ""])))))
    
    (test-case "font-xml"
      (check-xml (font-xml empty-font)
                 (xml (font ,(xml))))
      (check-xml (font-xml (make-font #:name         "Gill Sans"
                                      #:size         20
                                      #:color        (rgb .25 .5 .75)
                                      #:bold?        #t
                                      #:italic?      #t
                                      #:underline?   #t
                                      #:outline?     #t
                                      #:shadow?      #t
                                      #:strike?      #t
                                      #:superscript? #t))
                 (xml (font (name  (@ [val "Gill Sans"]))
                            (sz    (@ [val 20]))
                            (color (@ [rgb "FF3F7FBF"]))
                            (b)
                            (i)
                            (u)
                            (outline)
                            (shadow)
                            (strike)
                            (vertAlign (@ [val "superscript"]))))))
    
    (test-case "fill-xml"
      (check-xml (fill-xml empty-fill)
                 (xml (fill (patternFill (@ [patternType "none"])))))
      (check-xml (fill-xml (make-solid-fill (rgb .25 .5 .75)))
                 (xml (fill (patternFill (@ [patternType "solid"])
                                         (fgColor (@ [rgb "FF3F7FBF"]))
                                         (bgColor (@ [rgb "FF3F7FBF"]))))))
      (check-xml (fill-xml (make-pattern-fill (rgb .25 .5 .75)
                                              (rgb .5 .75 1)
                                              (pattern-type dark-trellis)))
                 (xml (fill (patternFill (@ [patternType "darkTrellis"])
                                         (fgColor (@ [rgb "FF3F7FBF"]))
                                         (bgColor (@ [rgb "FF7FBFFF"]))))))
      (check-xml (fill-xml (make-linear-gradient-fill
                            45
                            (list (make-gradient-stop 0   (rgb .25 .25 .25))
                                  (make-gradient-stop 0.5 (rgb .5  .5  .5))
                                  (make-gradient-stop 1   (rgb .75 .75 .75)))))
                 (xml (fill (gradientFill (@ [type "linear"] [degree "45"])
                                          (stop (@ [position "0.0"]) (color (@ [rgb "FF3F3F3F"])))
                                          (stop (@ [position "0.5"]) (color (@ [rgb "FF7F7F7F"])))
                                          (stop (@ [position "1.0"]) (color (@ [rgb "FFBFBFBF"])))))))
      (check-xml (fill-xml (make-path-gradient-fill
                            .1 .2 .3 .4
                            (list (make-gradient-stop 0   (rgb .25 .25 .25))
                                  (make-gradient-stop 0.5 (rgb .5  .5  .5))
                                  (make-gradient-stop 1   (rgb .75 .75 .75)))))
                 (xml (fill (gradientFill (@ [type "path"] [top "0.1"] [right "0.2"] [bottom "0.3"] [left "0.4"])
                                          (stop (@ [position "0.0"]) (color (@ [rgb "FF3F3F3F"])))
                                          (stop (@ [position "0.5"]) (color (@ [rgb "FF7F7F7F"])))
                                          (stop (@ [position "1.0"]) (color (@ [rgb "FFBFBFBF"]))))))))
    
    (test-case "border-xml"
      (check-xml (border-xml empty-border)
                 (xml (border ,(xml))))
      (check-xml (border-xml (make-border #:top      (make-line (border-style thin) (rgb 0 0 0))
                                          #:right    (make-line (border-style thin) (rgb .25 .25 .25))
                                          #:bottom   (make-line (border-style thin) (rgb .5 .5 .5))
                                          #:left     (make-line (border-style thin) (rgb .75 .75 .75))
                                          #:diagonal (make-line (border-style thin) (rgb 1 1 1))))
                 (xml (border (@ [diagonalDown "true"])
                              (left     (@ [style "thin"]) (color (@ [rgb "FFBFBFBF"])))
                              (right    (@ [style "thin"]) (color (@ [rgb "FF3F3F3F"])))
                              (top      (@ [style "thin"]) ,(xml))
                              (bottom   (@ [style "thin"]) (color (@ [rgb "FF7F7F7F"])))
                              (diagonal (@ [style "thin"]) (color (@ [rgb "FFFFFFFF"])))))))
    
    (test-case "style-xml"
      (check-xml (style-xml empty-style 1 2 3 4)
                 (xml (xf (@ [numFmtId 1] [fontId 2] [fillId 3] [borderId 4]) ,(xml))))
      (check-xml (style-xml (make-compiled-style
                             #:number-format (make-number-format "0%")
                             #:font          (make-font #:size 12)
                             #:fill          (make-solid-fill (rgb .1 .2 .3))
                             #:border        (make-border #:top (make-line (border-style thin)))
                             #:alignment     (make-alignment
                                              #:horizontal         (horizontal-alignment left)
                                              #:vertical           (vertical-alignment top)
                                              #:wrap?              #t
                                              #:shrink?            #t
                                              #:rotation           45
                                              #:reading-order      (reading-order left-to-right)
                                              #:justify-last-line? #t
                                              #:indent             1
                                              #:relative-indent    2)
                             #:hidden?       #t
                             #:locked?       #f)
                            3 4 5 6)
                 (xml (xf (@ [numFmtId          3]
                             [fontId            4]
                             [fillId            5]
                             [borderId          6]
                             [applyNumberFormat "true"]
                             [applyFont         "true"]
                             [applyFill         "true"]
                             [applyBorder       "true"]
                             [applyAlignment    "true"]
                             [applyProtection   "true"])
                          (alignment  (@ [horizontal      "left"]
                                         [vertical        "top"]
                                         [wrapText        "true"]
                                         [shrinkToFit     "true"]
                                         [textRotation    45]
                                         [readingOrder    1]
                                         [justifyLastLine "true"]
                                         [indent          1]
                                         [relativeIndent  2]))
                          (protection (@ [hidden "true"]
                                         [locked "false"]))))))
    
    (test-case "stylesheet-xml!"
      (check-xml
       (stylesheet-xml!
        (make-cache)
        (make-workbook
         (list (make-worksheet
                "Sheet1"
                (hc-append #:style
                           (make-compiled-style #:fill (make-solid-fill (rgb .1 .2 .3)))
                           1
                           (vc-append #:style
                                      (make-uncompiled-style
                                       (lambda (x y)
                                         (make-compiled-style #:font (make-font #:size y))))
                                      2 3 4)
                           5))
               (make-worksheet
                "Sheet2"
                (hc-append #:style
                           (make-uncompiled-style
                            (lambda (x y)
                              (make-compiled-style
                               #:border (make-border #:top    (and (= y 0) (make-line))
                                                     #:right  (and (= x 2) (make-line))
                                                     #:bottom (and (= y 2) (make-line))
                                                     #:left   (and (= x 0) (make-line))))))
                           1
                           (vc-append #:style
                                      (make-compiled-style #:font (make-font #:italic? #t))
                                      2 3 4)
                           5)))))
       (xml ,standalone-header-xml
            (styleSheet (@ [xmlns ,spreadsheetml-namespace])
                        (numFmts (@ [count "0"])
                                 ,(xml))
                        (fonts   (@ [count "5"])
                                 (font ,(xml))
                                 (font (sz (@ [val "0"])))
                                 (font (sz (@ [val "1"])))
                                 (font (sz (@ [val "2"])))
                                 (font (i)))
                        (fills   (@ [count "3"])
                                 (fill (patternFill (@ [patternType "none"])))
                                 (fill (patternFill (@ [patternType "gray125"])
                                                    (fgColor (@ [rgb "FF000000"]))
                                                    (bgColor (@ [rgb "FFFFFFFF"]))))
                                 (fill (patternFill (@ [patternType "solid"])
                                                    (fgColor (@ [rgb "FF19334C"]))
                                                    (bgColor (@ [rgb "FF19334C"])))))
                        (borders (@ [count "9"])
                                 (border ,(xml))
                                 (border (left   (@ [style "thin"]) ,(xml)))
                                 (border (top    (@ [style "thin"]) ,(xml)))
                                 (border (bottom (@ [style "thin"]) ,(xml)))
                                 (border (right  (@ [style "thin"]) ,(xml)))
                                 (border (left   (@ [style "thin"]) ,(xml))
                                         (top    (@ [style "thin"]) ,(xml)))
                                 (border (right  (@ [style "thin"]) ,(xml))
                                         (top    (@ [style "thin"]) ,(xml)))
                                 (border (left   (@ [style "thin"]) ,(xml))
                                         (bottom (@ [style "thin"]) ,(xml)))
                                 (border (right  (@ [style "thin"]) ,(xml))
                                         (bottom (@ [style "thin"]) ,(xml))))
                        (cellXfs (@ [count "14"])
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "0"]
                                        [fillId      "0"]
                                        [borderId    "0"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "0"]
                                        [fillId      "2"]
                                        [borderId    "0"]
                                        [applyFill   "true"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "1"]
                                        [fillId      "2"]
                                        [borderId    "0"]
                                        [applyFont   "true"]
                                        [applyFill   "true"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "2"]
                                        [fillId      "2"]
                                        [borderId    "0"]
                                        [applyFont   "true"]
                                        [applyFill   "true"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "3"]
                                        [fillId      "2"]
                                        [borderId    "0"]
                                        [applyFont   "true"]
                                        [applyFill   "true"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "0"]
                                        [fillId      "0"]
                                        [borderId    "1"]
                                        [applyBorder "true"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "4"]
                                        [fillId      "0"]
                                        [borderId    "2"]
                                        [applyFont   "true"]
                                        [applyBorder "true"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "4"]
                                        [fillId      "0"]
                                        [borderId    "0"]
                                        [applyFont   "true"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "4"]
                                        [fillId      "0"]
                                        [borderId    "3"]
                                        [applyFont   "true"]
                                        [applyBorder "true"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "0"]
                                        [fillId      "0"]
                                        [borderId    "4"]
                                        [applyBorder "true"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "0"]
                                        [fillId      "0"]
                                        [borderId    "5"]
                                        [applyBorder "true"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "0"]
                                        [fillId      "0"]
                                        [borderId    "6"]
                                        [applyBorder "true"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "0"]
                                        [fillId      "0"]
                                        [borderId    "7"]
                                        [applyBorder "true"]) ,(xml))
                                 (xf (@ [numFmtId    "0"]
                                        [fontId      "0"]
                                        [fillId      "0"]
                                        [borderId    "8"]
                                        [applyBorder "true"]) ,(xml)))
                        (dxfs (@ [count "0"]) ,(xml))))))))

; Provide statements -----------------------------

(provide xml-style-tests)
