#lang scheme/base

(require "base.ss")

(require scheme/string
         "struct-internal.ss")

; Basic conversion procedures --------------------

; natural natural [boolean] [boolean] -> string
(define (xy->ref x y [absolute-x? #f] [absolute-y? #f])
  (format "~a~a~a~a"
          (if absolute-x? "$" "")
          (x->col x) 
          (if absolute-y? "$" "")
          (add1 y)))

; (U worksheet #f) natural natural [boolean] [boolean] -> string
(define (sheet+xy->ref sheet x y [absolute-x? #f] [absolute-y? #f])
  (if sheet
      (format "~a!~a" (escape-worksheet-name (worksheet-name sheet)) (xy->ref x y absolute-x? absolute-y?))
      (xy->ref x y absolute-x? absolute-y?)))

; string -> natural natural
(define (ref->xy ref)
  (let-values ([(sheet x y) (ref->sheet+xy ref)])
    (values x y)))

; string -> (U string #f) natural natural
(define (ref->sheet+xy ref)
  (match (regexp-match #rx"^(\\[?([^\\]*)\\]?!)?\\$?([a-zA-Z][a-zA-Z]*)[$]?([0-9][0-9]*)$" ref)
    [(list _ _ sheet col row)
     (values (and sheet (unescape-worksheet-name sheet)) (col->x col) (row->y row))]))

; natural -> string
; string -> natural
;
; Procedures for converting between zero-based x indices and "AA" style column indices.
; They ain't pretty but they seem to work.
(define-values (x->col col->x)
  (let ([a/int (char->integer #\a)]
        [A/int (char->integer #\A)])
    (values (lambda (x)
              ; (listof char)
              (define chars
                (let loop ([num x])
                  (if (zero? num)
                      (list #\A)
                      (let ([hi (quotient num 26)]
                            [lo (remainder num 26)])
                        (cons (integer->char (+ A/int lo))
                              (if (zero? hi)
                                  null
                                  (loop (sub1 hi))))))))
              ; string
              (apply string (reverse chars)))
            (lambda (col)
              ; natural natural
              (define-values (accum base)
                (for/fold ([accum 0] [base  1])
                          ([char (in-list (reverse (string->list col)))])
                          (let ([digit (add1 (- (char->integer char)
                                                (if (char-upper-case? char)
                                                    A/int a/int)))])
                            (values (+ accum (* digit base))
                                    (* base 26)))))
              ; natural
              (sub1 accum)))))

; natural -> string
; string -> natural
;
; Procedures for converting between zero-based y indices and "1"-based row indices.
(define-values (y->row row->y)
  (values (lambda (y)
            (number->string (add1 y)))
          (lambda (row)
            (sub1 (string->number row)))))

; Range addresses --------------------------------

; range [worksheet] natural natural -> string
(define range-address
  (case-lambda
    [(range x0 y0)
     (range-address range #f x0 y0)]
    [(range sheet x0 y0)
     (let ([w (range-width range)]
           [h (range-height range)])
       (if (and (= w 1) (= h 1))
           (if sheet
               (sheet+xy->ref sheet x0 y0)
               (xy->ref x0 y0))
           (format "~a:~a"
                   (if sheet
                       (sheet+xy->ref sheet x0 y0)
                       (xy->ref x0 y0))
                   (if sheet
                       (sheet+xy->ref sheet
                                      (sub1 (+ x0 w))
                                      (sub1 (+ y0 h)))
                       (xy->ref (sub1 (+ x0 w))
                                (sub1 (+ y0 h)))))))]))

; (listof (list range natural natural)) -> string
(define (range-addresses specs)
  (string-join (for/list ([spec (in-list specs)])
                 (apply range-address spec))
               " "))

; Helpers ----------------------------------------

; string -> string
(define (escape-worksheet-name name)
  (format "'~a'" (regexp-replace* #rx"'" name "''")))

; string -> string
(define (unescape-worksheet-name name)
  (match (regexp-match #rx"^'(.*)'$" name)
    [(list _ name) (regexp-replace* #rx"''" name "'")]
    [_             (regexp-replace* #rx"''" name "'")]))

; Provide statements -----------------------------

(provide/contract
 [xy->ref         (->* (natural-number/c natural-number/c)
                       (boolean? boolean?)
                       string?)]
 [sheet+xy->ref   (->* ((or/c worksheet? #f) natural-number/c natural-number/c)
                       (boolean? boolean?)
                       string?)]
 [ref->xy         (-> string? (values natural-number/c natural-number/c))]
 [ref->sheet+xy   (-> string? (values (or/c string? #f) natural-number/c natural-number/c))]
 [x->col          (-> natural-number/c string?)]
 [col->x          (-> string? natural-number/c)]
 [y->row          (-> natural-number/c string?)]
 [row->y          (-> string? natural-number/c)]
 [range-address   (case-> (-> range? natural-number/c natural-number/c string?)
                          (-> range? worksheet? natural-number/c natural-number/c string?))]
 [range-addresses (-> (listof (list/c range? natural-number/c natural-number/c)) string?)])
