#lang scheme/base

(require "base.ss")

(require "struct.ss")

; xml
(define standalone-header-xml
  (xml (!raw "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>")))

; Reserved part IDs ------------------------------

(define stylesheet-part-id "stylesPart")

; Namespaces -------------------------------------

(define content-types-namespace            "http://schemas.openxmlformats.org/package/2006/content-types")
(define package-relationships-namespace    "http://schemas.openxmlformats.org/package/2006/relationships")
(define spreadsheetml-namespace            "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
(define workbook-namespace                 "http://schemas.openxmlformats.org/officeDocument/2006/relationships")
(define worksheet-namespace                "http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet")
(define stylesheet-namespace               "http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles")

; Content types ----------------------------------

(define package-relationships-content-type "application/vnd.openxmlformats-package.relationships+xml")
(define workbook-content-type              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml")
(define worksheet-content-type             "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml")
(define stylesheet-content-type            "application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml")

; Relationships ----------------------------------

(define office-document-relationship       "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument")

; Procedures -------------------------------------

; -> (-> natural)
(define (make-counter initial)
  (let ([counter initial])
    (lambda ()
      (begin0 counter
              (set! counter (add1 counter))))))

;  range
;  (range accum natural natural -> accum)
;  (part accum -> accum)
;  (range accum natural natural -> void)
;  accum
;  [natural]
;  [natural]
; ->
;  void
;
; Computes the compiled style in each (x,y) position in the range
; and calls accumulate! with the style and the cell in that square
; (if any).
(define (range-for-each range compose/range compose/part consume! accum0 [x0 0] [y0 0])
  (let ([accum (compose/range range accum0 x0 y0)])
    
    ; Recurse to children:
    (when (union? range)
      (let ([parts (union-parts range)])
        (for ([part (in-list parts)])
          (range-for-each (part-range part)
                          compose/range
                          compose/part
                          consume!
                          (compose/part part accum)
                          (+ x0 (part-dx part))
                          (+ y0 (part-dy part))))))
    
    ; Consume values for this range:
    (consume! range accum x0 y0)))

; Provide statements -----------------------------

(provide/contract
 [standalone-header-xml              xml?]
 [stylesheet-part-id                 string?]
 [content-types-namespace            string?]
 [package-relationships-namespace    string?]
 [spreadsheetml-namespace            string?]
 [workbook-namespace                 string?]
 [worksheet-namespace                string?]
 [stylesheet-namespace               string?]
 [package-relationships-content-type string?]
 [workbook-content-type              string?]
 [worksheet-content-type             string?]
 [stylesheet-content-type            string?]
 [office-document-relationship       string?])

(provide/contract
 [make-counter   (-> natural-number/c (-> natural-number/c))]
 [range-for-each (->* (range? 
                       (-> range? any/c natural-number/c natural-number/c any)
                       (-> part? any/c any)
                       (-> range? any/c natural-number/c natural-number/c void?)
                       any/c)
                      (natural-number/c natural-number/c)
                      void?)])
