#lang scheme/base

(require "base.ss")

; (struct)
(define-struct data () #:transparent #:mutable)

; (struct symbol)
(define-struct (package-part data) (id) #:transparent #:mutable)

; (struct symbol (listof worksheet))
(define-struct (workbook package-part) (sheets) #:transparent #:mutable)

; (struct symbol (U string #f) range)
(define-struct (worksheet package-part) (name data) #:transparent #:mutable)

; (struct)
(define-struct (range data) () #:transparent #:mutable)

; (struct any)
(define-struct (cell range) (value) #:transparent #:mutable)

; (struct (listof part) natural natural)
(define-struct (union range) (parts width height) #:transparent)

; (struct range natural natural)
(define-struct (part data) (range dx dy) #:transparent)

; Provide statements -----------------------------

(provide/contract
 [struct data                     (#;[parent  (or/c data? #f)])]
 [struct package-part             (#;[parent  (or/c data? #f)]
                                   [id      symbol?])]
 [struct (workbook package-part)  (#;[parent  #f]
                                   [id      symbol?]
                                   [sheets  (listof worksheet?)])]
 [struct (worksheet package-part) (#;[parent  (or/c workbook? #f)]
                                   [id      symbol?]
                                   [name    string?]
                                   [data    range?])]
 [struct (range data)             (#;[parent  (or/c worksheet? range? #f)])]
 [struct (cell range)             (#;[parent  (or/c worksheet? range? #f)]
                                   [value   any/c])]
 [struct (union range)            (#;[parent  (or/c worksheet? range? #f)]
                                   [parts   (listof part?)]
                                   [width   natural-number/c]
                                   [height  natural-number/c])]
 [struct (part data)              (#;[parent  (or/c union? #f)]
                                   [range   range?]
                                   [dx      natural-number/c]
                                   [dy      natural-number/c])])