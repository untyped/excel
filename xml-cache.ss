#lang scheme/base

(require (planet untyped/unlib:3/hash)
         "base.ss"
         "ref.ss"
         "struct.ss")

; (struct (alistof string (alistof natural (alistof natural cell)))
;         (hashof cell (list string natural natural))
;         (hashof (U style style-component) natural)
;         (hashof cell natural))
;
; A cell cache containing two data structures that are used to render a workbook;
;   - a forward lookup optimised for iterating over the workbook;
;   - a reverse lookup optimised for random access, retrieving cell references.
(define-struct cache (forward-lookup reverse-lookup style-lookup cell-style-lookup) #:transparent)

; Constructor ------------------------------------

; workbook -> cache
(define (create-cache book)
  (let* ([forward (create-forward-lookup/book book)]
         [reverse (create-reverse-lookup forward)])
    (make-cache forward reverse (make-hash) (make-hasheq))))

; cache worksheet -> (alistof natural (alistof natural cell))
(define (cache-worksheet-data cache sheet)
  (let ([pair (assq sheet (cache-forward-lookup cache))])
    (if pair (cdr pair) (error "sheet not found" sheet))))

; cache worksheet cell [boolean] [boolean] -> string
(define (cache-ref cache sheet cell [abs-x? #f] [abs-y? #f])
  (match (hash-ref (cache-reverse-lookup cache) cell)
    [(list cell-sheet cell-x cell-y)
     (if (eq? sheet cell-sheet)
         (xy->ref cell-x cell-y abs-x? abs-y?)
         (sheet+xy->ref cell-sheet cell-x cell-y abs-x? abs-y?))]
    [#f (error "cell not found" cell)]))

; Helpers ----------------------------------------

; workbook -> (alistof string (alistof natural (alistof natural cell)))
(define (create-forward-lookup/book book)
  (for/list ([sheet (in-list (workbook-sheets book))])
    (cons sheet (create-forward-lookup/sheet sheet))))

; worksheet -> (alistof natural (alistof natural cell))
(define (create-forward-lookup/sheet sheet)
  (create-forward-lookup/range (worksheet-data sheet) 0 0))

; range natural natural -> (alistof natural (alistof natural cell))
(define (create-forward-lookup/range range x0 y0)
  (let* ([cache      (make-hasheq)]
         [row-ref    (lambda (y)
                       (hash-ref cache y (lambda ()
                                           (let ([row (make-hasheq)])
                                             (hash-set! cache y row)
                                             row))))]
         [cache-set! (lambda (x y cell)
                       (let ([row (row-ref y)])
                         (if (hash-ref row x #f)
                             (error (format "(~a,~a) already contains a cell" x y)
                                    (hash-ref row x #f))
                             (hash-set! row x cell))))])
    (let loop ([x x0] [y y0] [curr range])
      (match curr
        [(? union?) (for ([part (in-list (union-parts curr))])
                      (loop x y part))]
        [(? part?)  (loop (+ x (part-dx curr))
                          (+ y (part-dy curr))
                          (part-range curr))]
        [(? cell?)  (cache-set! x y curr)]))
    (for/list ([y (in-list (sort (hash-keys cache) <))])
      (let ([row (hash-ref cache y)])
        (cons y (for/list ([x (in-list (sort (hash-keys row) <))])
                  (let ([cell (hash-ref row x)])
                    (cons x cell))))))))

;  (alistof string (alistof natural (alistof natural cell)))
; ->
;  (hashof cell (list string natural natural))
(define (create-reverse-lookup forward)
  (let ([ans (make-hasheq)])
    (for ([item (in-list forward)])
      (let ([sheet (car item)]
            [data  (cdr item)])
        (for ([item (in-list data)])
          (let ([y   (car item)]
                [row (cdr item)])
            (for ([item (in-list row)])
              (let ([x    (car item)]
                    [cell (cdr item)])
                (hash-set! ans cell (list sheet x y))))))))
    ans))

; cache style [(U natural #f)] -> (U natural #f)
(define cache-style-ref
  (case-lambda
    [(cache style)         (hash-ref (cache-style-lookup cache) style)]
    [(cache style default) (hash-ref (cache-style-lookup cache) style default)]))

; cache style natural -> void
(define (cache-style-set! cache style val)
  (hash-set! (cache-style-lookup cache) style val))

; cache cell -> (U natural #f)
(define (cache-cell-style-ref cache cell)
 (hash-ref (cache-cell-style-lookup cache) cell #f))

; cache style natural -> void
(define (cache-cell-style-set! cache cell val)
  (hash-set! (cache-cell-style-lookup cache) cell val))

; Provide statements -----------------------------

(provide/contract
 [rename create-cache make-cache (-> workbook? cache?)]
 [cache?                         (-> any/c boolean?)]
 [cache-forward-lookup           (-> cache? (or/c pair? null?))]
 [cache-reverse-lookup           (-> cache? hash?)]
 [cache-worksheet-data           (-> cache? worksheet? (or/c pair? null?))]
 [cache-ref                      (->* (cache? worksheet? cell?) (boolean? boolean?) string?)]
 [cache-style-ref                (->* (cache? any/c) ((or/c natural-number/c #f)) (or/c natural-number/c #f))]
 [cache-style-set!               (-> cache? any/c natural-number/c void?)]
 [cache-cell-style-ref           (-> cache? cell? (or/c natural-number/c #f))]
 [cache-cell-style-set!          (-> cache? cell? natural-number/c void?)])