#lang scheme/base

(require "base.ss")

; (struct symbol)
(define-struct part (id) #:transparent #:mutable)

; (struct symbol (listof worksheet))
(define-struct (workbook part) (sheets) #:transparent #:mutable)

; (struct symbol (U string #f) (hasheqof natural (hasheqof natural cell)))
(define-struct (worksheet part) (name data) #:transparent #:mutable)

; (struct (U worksheet #f) (U natural #f) (U natural #f) any)
(define-struct cell (sheet x y value) #:transparent #:mutable)

; Provide statements -----------------------------

(provide/contract
 [struct part             ([id     symbol?])]
 [struct (workbook part)  ([id     symbol?]
                           [sheets (listof worksheet?)])]
 [struct (worksheet part) ([id     symbol?]
                           [name   string?]
                           [data   (and/c hash? hash-eq?)])]
 [struct cell             ([sheet  (or/c worksheet? #f)]
                           [x      (or/c natural-number/c #f)]
                           [y      (or/c natural-number/c #f)]
                           [value  any/c])])