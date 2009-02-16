#lang scheme/base

(require "base.ss")

(require "struct.ss")

; Filenames --------------------------------------

;  [#:relative-to (U path #f)]
;  [#:absolute? boolean]
; ->
;  path
(define (content-types-path #:relative-to [relative-to-path #f] #:absolute? [absolute? #f])
  (format-path (build-path "[Content_Types].xml")
               #:relative-to relative-to-path
               #:absolute?   absolute?))

;  [#:relative-to (U path #f)]
;  [#:absolute? boolean]
; ->
;  path
(define (package-relationships-path #:relative-to [relative-to-path #f] #:absolute? [absolute? #f])
  (format-path (build-path "_rels/.rels")
               #:relative-to relative-to-path
               #:absolute?   absolute?))

;  workbook
;  [#:relative-to (U path #f)]
;  [#:absolute? boolean]
; ->
;  path
(define (workbook-relationships-path book #:relative-to [relative-to-path #f] #:absolute? [absolute? #f])
  (format-path (build-path "xl/_rels/workbook.xml.rels")
               #:relative-to relative-to-path
               #:absolute?   absolute?))

;  workbook
;  [#:relative-to (U path #f)]
;  [#:absolute? boolean]
; ->
;  path
(define (workbook-styles-path book #:relative-to [relative-to-path #f] #:absolute? [absolute? #f])
  (format-path (build-path "xl/styles.xml")
               #:relative-to relative-to-path
               #:absolute?   absolute?))

;  package-part
;  [#:relative-to (U path #f)]
;  [#:absolute? boolean]
; ->
;  path
(define (package-part-path part #:relative-to [relative-to-path #f] #:absolute? [absolute? #f])
  (format-path (match part
                 [(? workbook? part)  (workbook-path part)]
                 [(? worksheet? part) (worksheet-path part)])
               #:relative-to relative-to-path
               #:absolute?   absolute?))

; Helpers ----------------------------------------

;  path
;  [#:relative-to (U path #f)]
;  [#:absolute? boolean]
; ->
;  path
(define (format-path complete-path #:relative-to [relative-to-path #f] #:absolute? [absolute? #f])
  (let ([relative-path (if relative-to-path
                           (find-relative-path
                            (build-path "/" relative-to-path)
                            (build-path "/" complete-path))
                           complete-path)])
    (if absolute?
        (build-path "/" relative-path)
        relative-path)))

; workbook -> relative-path
(define (workbook-path book)
  (build-path "xl" "workbook.xml"))

; worksheet -> relative-path
(define (worksheet-path sheet)
  (build-path "xl/worksheets" (format "~a.xml" (package-part-id sheet))))

; Provide statements -----------------------------

; contract
(define relative-path/c
  (and/c path? relative-path?))

(provide/contract
 [content-types-path          (->* () (#:relative-to (or/c relative-path/c #f) #:absolute? boolean?) path?)]
 [package-relationships-path  (->* () (#:relative-to (or/c relative-path/c #f) #:absolute? boolean?) path?)]
 [workbook-relationships-path (->* (workbook?) (#:relative-to (or/c relative-path/c #f) #:absolute? boolean?) path?)]
 [workbook-styles-path        (->* (workbook?) (#:relative-to (or/c relative-path/c #f) #:absolute? boolean?) path?)]
 [package-part-path           (->* (package-part?) (#:relative-to (or/c relative-path/c #f) #:absolute? boolean?) path?)])
