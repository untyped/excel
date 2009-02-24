#lang scheme/base

(require "base.ss")

(require "formula-render.ss"
         "ref.ss"
         "struct.ss"
         "xml-cache.ss"
         "xml-internal.ss")

; cache worksheet -> xml
(define (worksheet-xml cache sheet)
  (xml ,standalone-header-xml
       (worksheet (@ [xmlns   ,spreadsheetml-namespace]
                     [xmlns:r ,workbook-namespace])
                  ,(sheet-data-xml cache sheet)
                  ,(sheet-protection-xml cache sheet)
                  ,(cf+validation-xml cache sheet))))

; cache worksheet -> xml
(define (sheet-data-xml cache sheet)
  (xml (sheetData ,@(for/list ([y (in-range (range-height (worksheet-data sheet)))])
                      (let ([cells (for/list ([x (in-range (range-width (worksheet-data sheet)))])
                                     (let-values ([(cell s) (cache-value-ref cache sheet x y)])
                                       (opt-xml s
                                         ,(let ([r (xy->ref x y)])
                                            (if cell
                                                (match (cell-value cell)
                                                  [#t              (xml (c (@ [r ,r] [s ,s] [t "b"]) (v 1)))]
                                                  [#f              (xml (c (@ [r ,r] [s ,s])))]
                                                  [(? number? n)   (xml (c (@ [r ,r] [s ,s]) (v ,n)))]
                                                  [(? string? str) (xml (c (@ [r ,r] [s ,s] [t "inlineStr"]) (is (t ,str))))]
                                                  [(? symbol? sym) (xml (c (@ [r ,r] [s ,s] [t "inlineStr"]) (is (t ,sym))))]
                                                  [(? bytes?  byt) (xml (c (@ [r ,r] [s ,s] [t "inlineStr"]) (is (t ,byt))))]
                                                  [(? formula? f)  (xml (c (@ [r ,r] [s ,s])
                                                                           (f (@ ,@(if (formula-array? f)
                                                                                       (xml-attrs [t "array"] [aca "true"] [ref ,r])
                                                                                       (xml-attrs)))
                                                                              ,(expression->string cache sheet cell x y (formula-expr f)))))])
                                                (xml (c (@ [r ,r] [s ,s]))))))))])
                        (opt-xml (not (andmap xml-empty? cells))
                          (row (@ [r ,(y->row y)])
                               ,@cells)))))))

; cache worksheet -> xml
(define (sheet-protection-xml cache sheet)
  (xml (sheetProtection (@ [autoFilter          ,(if (worksheet-auto-filter-lock?             sheet) "true" "false")]
                           [deleteColumns       ,(if (worksheet-delete-columns-lock?          sheet) "true" "false")]
                           [deleteRows          ,(if (worksheet-delete-rows-lock?             sheet) "true" "false")]
                           [formatCells         ,(if (worksheet-format-cells-lock?            sheet) "true" "false")]
                           [formatColumns       ,(if (worksheet-format-columns-lock?          sheet) "true" "false")]
                           [formatRows          ,(if (worksheet-format-rows-lock?             sheet) "true" "false")]
                           [insertColumns       ,(if (worksheet-insert-columns-lock?          sheet) "true" "false")]
                           [insertHyperlinks    ,(if (worksheet-insert-hyperlinks-lock?       sheet) "true" "false")]
                           [insertRows          ,(if (worksheet-insert-rows-lock?             sheet) "true" "false")]
                           [objects             ,(if (worksheet-objects-lock?                 sheet) "true" "false")]
                           [pivotTables         ,(if (worksheet-pivot-tables-lock?            sheet) "true" "false")]
                           [scenarios           ,(if (worksheet-scenarios-lock?               sheet) "true" "false")]
                           [selectLockedCells   ,(if (worksheet-locked-cell-selection-lock?   sheet) "true" "false")]
                           [selectUnlockedCells ,(if (worksheet-unlocked-cell-selection-lock? sheet) "true" "false")]
                           [sheet               ,(if (worksheet-sheet-lock?                   sheet) "true" "false")]
                           [sort                ,(if (worksheet-sort-lock?                    sheet) "true" "false")]))))

; cache worksheet -> xml
(define (cf+validation-xml cache sheet)
  ; (box (listof xml))
  (define cf-accum         (box null))
  (define validation-accum (box null))
  
  (range-for-each
   ; range
   (worksheet-data sheet)
   ; compose/range : range #f natural natural -> #f
   (lambda (range _ x y) #f)
   ; compose/part  : part #f -> #f
   (lambda (part _) #f)
   ; consume!      : range #f natural natural -> void
   (lambda (range _ x0 y0)
     (unless (null? (range-conditional-formats range))
       (set-box! cf-accum (cons (conditional-format-xml cache sheet range x0 y0) (unbox cf-accum))))
     (when (range-validation-rule range)
       (set-box! validation-accum (cons (validation-rule-xml cache sheet range x0 y0) (unbox validation-accum)))))
   ; accum0        : #f
   #f)
  
  (xml ,@(reverse (unbox cf-accum))
       ,(opt-xml (pair? (unbox validation-accum))
          (dataValidations (@ [count ,(length (unbox validation-accum))])
                           ,@(reverse (unbox validation-accum))))))

; cache worksheet range natural natural -> xml
(define (conditional-format-xml cache sheet range x0 y0)
  (xml (conditionalFormatting
        (@ [sqref ,(format "~a:~a"
                           (xy->ref x0 y0)
                           (xy->ref (sub1 (+ x0 (range-width range)))
                                    (sub1 (+ y0 (range-height range)))))])
        ,@(for/list ([cf (in-list (range-conditional-formats range))])
            (match cf
              [(struct conditional-format (formula style priority))
               (xml (cfRule (@ [type     "expression"]
                               [dxfId    ,(cache-diff-style-ref cache style)]
                               [priority ,priority])
                            (formula ,(expression->string cache sheet range x0 y0 (formula-expr formula)))))])))))

; cache worksheet range natural natural -> xml
(define (validation-rule-xml cache sheet range x0 y0)
  (match (range-validation-rule range)
    [(struct validation-rule (formula errorStyle errorTitle error promptTitle prompt))
     (xml (dataValidation
           (@ [sqref            ,(format "~a:~a"
                                         (xy->ref x0 y0)
                                         (xy->ref (sub1 (+ x0 (range-width range)))
                                                  (sub1 (+ y0 (range-height range)))))]
              [type             "custom"]
              [allowBlank       "1"]
              [showInputMessage ,(if (or promptTitle prompt) "1" "0")]
              [showErrorMessage ,(if (or errorTitle  error)  "1" "0")]
              ,(opt-xml-attr errorStyle)
              ,(opt-xml-attr errorTitle)
              ,(opt-xml-attr error)
              ,(opt-xml-attr promptTitle)
              ,(opt-xml-attr prompt))
           (formula1 ,(expression->string cache sheet range x0 y0 (formula-expr formula)))))]))

; Provide statements -----------------------------

(provide/contract
 [worksheet-xml (-> cache? worksheet? xml?)])
