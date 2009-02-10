#lang scheme/base

(require "base.ss"
         "struct-style-internal.ss")

; (struct)
(define-struct data () #:transparent #:mutable)

; (struct symbol)
(define-struct (package-part data) (id) #:transparent #:mutable)

; (struct symbol (listof worksheet))
(define-struct (workbook package-part) (sheets) #:transparent #:mutable)

; (struct symbol (U string #f) range)
(define-struct (worksheet package-part) (name data) #:transparent #:mutable)

; (struct (U style #f))
(define-struct (range data) (style) #:transparent #:mutable)

; (struct (listof style) any)
(define-struct (cell range) (value) #:transparent #:mutable)

; (struct (listof style) (listof part) natural natural)
(define-struct (union range) (parts width height) #:transparent)

; (struct range natural natural)
(define-struct (part data) (range dx dy) #:transparent)

; Provide statements -----------------------------

(provide/contract
 [struct data                     ()]
 [struct package-part             ([id            symbol?])]
 [struct (workbook package-part)  ([id            symbol?]
                                   [sheets        (listof worksheet?)])]
 [struct (worksheet package-part) ([id            symbol?]
                                   [name          string?]
                                   [data          range?])]
 [struct (range data)             ([style         (or/c style? #f)])]
 [struct (cell range)             ([style         (or/c style? #f)]
                                   [value         any/c])]
 [struct (union range)            ([style         (or/c style? #f)]
                                   [parts         (listof part?)]
                                   [width         natural-number/c]
                                   [height        natural-number/c])]
 [struct (part data)              ([range         range?]
                                   [dx            natural-number/c]
                                   [dy            natural-number/c])])
