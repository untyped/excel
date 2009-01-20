#lang scheme/base

(require (planet untyped/unlib:3/hash)
         (planet untyped/unlib:3/symbol)
         "base.ss"
         "ref.ss"
         "struct-internal.ss")

; Workbook wrappers ------------------------------

;  [#:id symbol]
;  (listof worksheet)
; ->
;  workbook
(define (create-workbook #:id [id (gensym/interned 'book)] [sheets null])
  (make-workbook id sheets))

; Worksheet wrappers -----------------------------

;  [#:id symbol]
;  [(U string #f)]
; ->
;  workbook
(define (create-worksheet #:id [id (gensym/interned 'sheet)] [name #f])
  (make-worksheet id name (make-hasheq)))

; sheet -> (listof natural)
(define (worksheet-y-indices sheet)
  (sort (hash-keys (worksheet-data sheet)) <))

; sheet natural -> (listof natural)
(define (worksheet-x-indices sheet y)
  (let ([row (hash-ref (worksheet-data sheet) y #f)])
    (if row
        (sort (hash-keys row) <)
        null)))

; worksheet string -> (U cell #f)
; worksheet natural natural -> (U cell #f)
(define worksheet-ref
  (case-lambda
    [(sheet ref) (worksheet-ref/ref sheet ref)]
    [(sheet x y) (worksheet-ref/xy sheet x y)]))

; worksheet string -> (U cell #f)
(define (worksheet-ref/ref sheet ref)
  (let-values ([(x y) (ref->xy ref)])
    (worksheet-ref/xy sheet x y)))

; worksheet natural natural -> (U cell #f)
(define (worksheet-ref/xy sheet x y)
  (let* ([data (worksheet-data sheet)]
         [row  (hash-ref data y #f)])
    (and row (hash-ref row x #f))))

; worksheet string cell -> cell
; worksheet natural natural cell -> cell
(define worksheet-set!
  (case-lambda
    [(sheet ref cell) (worksheet-set!/ref sheet ref cell)]
    [(sheet x y cell) (worksheet-set!/xy sheet x y cell)]))

; worksheet string cell -> cell
(define (worksheet-set!/ref sheet ref cell)
  (let-values ([(x y) (ref->xy ref)])
    (worksheet-set!/xy sheet x y cell)))

; worksheet natural natural cell -> cell
(define (worksheet-set!/xy sheet x y cell)
  
  ; hasheq natural -> hasheq
  (define (add-row! data y)
    (let ([row (make-hasheq)])
      (hash-set! data y row)
      row))
  
  ; (hasheqof natural (hasheqof natural cell))
  (define data
    (worksheet-data sheet))
  
  ; (hasheqof natural cell)
  (define row
    (hash-ref data y (cut add-row! data y)))
  
  (when (cell-sheet cell)
    (worksheet-remove! (cell-sheet cell) cell))
  
  (hash-set! row x cell)
  (set-cell-x! cell x)
  (set-cell-y! cell y)
  (set-cell-sheet! cell sheet)
  
  cell)

; worksheet cell -> void
(define (worksheet-remove! sheet cell)
  ; natural
  ; natural
  (define-values (x y)
    (values (cell-x cell)
            (cell-y cell)))
  
  ; (hasheqof natural (hasheqof natural cell))
  (define data
    (worksheet-data sheet))
  
  ; (hasheqof natural cell)
  (define row
    (hash-ref data y))
  
  (unless (eq? (hash-ref row x) cell)
    (error "cell is not part of worksheet: ~s ~s" cell sheet))
  
  (hash-remove! row x)
  (when (zero? (hash-count row))
    (hash-remove! data y))
  (set-cell-sheet! cell #f)
  (set-cell-x! cell #f)
  (set-cell-y! cell #f))

; Cell wrappers ----------------------------------

; any -> cell
(define (create-cell value)
  (make-cell #f #f #f value))

; Provide statements -----------------------------

(provide (except-out (all-from-out "struct-internal.ss")
                     make-workbook
                     make-worksheet
                     set-worksheet-data!
                     worksheet-data
                     make-cell
                     set-cell-sheet!
                     set-cell-x!
                     set-cell-y!))

(define nat/c natural-number/c)

(provide/contract
 [rename create-workbook  make-workbook  (->* () (#:id symbol? (listof worksheet?)) workbook?)]
 [rename create-worksheet make-worksheet (->* () (#:id symbol? string?) worksheet?)]
 [worksheet-y-indices                    (-> worksheet? (listof nat/c))]
 [worksheet-x-indices                    (-> worksheet? nat/c (listof nat/c))]
 [worksheet-ref                          (case-> (-> worksheet? string? (or/c cell? #f))
                                                 (-> worksheet? nat/c nat/c (or/c cell? #f)))]
 [worksheet-ref/xy                       (-> worksheet? nat/c nat/c (or/c cell? #f))]
 [worksheet-ref/ref                      (-> worksheet? string? (or/c cell? #f))]
 [worksheet-set!                         (case-> (-> worksheet? string? cell? cell?)
                                                 (-> worksheet? nat/c nat/c cell? cell?))]
 [worksheet-set!/xy                      (-> worksheet? nat/c nat/c cell? cell?)]
 [worksheet-set!/ref                     (-> worksheet? string? cell? cell?)]
 [worksheet-remove!                      (-> worksheet? cell? void?)]
 [rename create-cell make-cell           (-> any/c cell?)])