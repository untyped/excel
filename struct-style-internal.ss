#lang scheme/base

(require "base.ss")

; (struct natural natural natural natural)
; Each component must be in the range [0,15].
(define-struct color (r g b a) #:transparent)

; (struct (U string #f))
; A code of #f means no formatting (will not override other formats when composing).
; A code of "" means "general" formatting (will override other formats when composing).
(define-struct number-format (code) #:transparent)

; (struct (U string #f)
;         (U natural #f)
;         (U color #f)
;         (U boolean void)
;         (U boolean void)
;         (U boolean void)
;         (U boolean void)
;         (U boolean void)
;         (U boolean void))
(define-struct font
  (name size color bold-raw italic-raw underline-raw outline-raw shadow-raw strike-raw superscript-raw subscript-raw)
  #:transparent)

; (struct (U number-format #f) (U font #f) (U boolean void) (U boolean void))
(define-struct style (number-format font hidden-raw locked-raw) #:transparent)

; Provide statements -----------------------------

(provide/contract
 [struct color                    ([r               (integer-in 0 15)]
                                   [g               (integer-in 0 15)]
                                   [b               (integer-in 0 15)]
                                   [a               (integer-in 0 15)])]
 [struct number-format            ([code            (or/c string? #f)])]
 [struct font                     ([name            (or/c string? #f)]
                                   [size            (or/c string? #f)]
                                   [color           (or/c string? #f)]
                                   [bold-raw        (or/c boolean? void?)]
                                   [italic-raw      (or/c boolean? void?)]
                                   [underline-raw   (or/c boolean? void?)]
                                   [outline-raw     (or/c boolean? void?)]
                                   [shadow-raw      (or/c boolean? void?)]
                                   [strike-raw      (or/c boolean? void?)]
                                   [superscript-raw (or/c boolean? void?)]
                                   [subscript-raw   (or/c boolean? void?)])]
 [struct style                    ([number-format   (or/c number-format? #f)]
                                   [font            (or/c font? #f)]
                                   [hidden-raw      (or/c boolean? void?)]
                                   [locked-raw      (or/c boolean? void?)])])
