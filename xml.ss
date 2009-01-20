#lang scheme/base

(require "base.ss"
         "path.ss"
         "ref.ss"
         "struct.ss"
         "formula/formula.ss")

; xml
(define standalone-header-xml
  (xml (!raw "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>")))

; Namespaces -------------------------------------

(define content-types-namespace            "http://schemas.openxmlformats.org/package/2006/content-types")
(define package-relationships-namespace    "http://schemas.openxmlformats.org/package/2006/relationships")
(define spreadsheetml-namespace            "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
(define workbook-namespace                 "http://schemas.openxmlformats.org/officeDocument/2006/relationships")
(define worksheet-namespace                "http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet")

; Content types ----------------------------------

(define package-relationships-content-type "application/vnd.openxmlformats-package.relationships+xml")
(define workbook-content-type              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml")
(define worksheet-content-type             "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml")

; Relationships ----------------------------------

(define office-document-relationship       "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument")

; Content ----------------------------------------

; workbook -> xml
(define (content-types-xml book)
  (xml ,standalone-header-xml
       (Types (@ [xmlns ,content-types-namespace])
              (Default (@ [Extension "rels"] [ContentType ,package-relationships-content-type]))
              ,@(for/list ([part (in-list (cons book (workbook-sheets book)))])
                  (xml (Override (@ [PartName     ,(path->string (part-path part #:absolute? #t))]
                                    [ContentType  ,(match part
                                                     [(? workbook?)  workbook-content-type]
                                                     [(? worksheet?) worksheet-content-type])])))))))

; workbook -> xml
(define (package-relationships-xml book)
  (xml ,standalone-header-xml
       (Relationships (@ [xmlns ,package-relationships-namespace])
                      (Relationship (@ [Id     ,(part-id book)]
                                       [Type   ,office-document-relationship]
                                       [Target ,(path->string (part-path book))])))))

; workbook -> xml
(define (workbook-xml book)
  (xml ,standalone-header-xml
       (workbook (@ [xmlns   ,spreadsheetml-namespace]
                    [xmlns:r ,workbook-namespace])
                 (sheets ,@(for/list ([sheet (in-list (workbook-sheets book))]
                                      [index (in-naturals 1)])
                             (xml (sheet (@ [name    ,(worksheet-name sheet)]
                                            [sheetId ,index]
                                            [r:id    ,(part-id sheet)]))))))))

; workbook -> xml
(define workbook-relationships-xml
  (let ([xl-path (build-path "xl")])
    (lambda (book)
      (xml ,standalone-header-xml
           (Relationships
            (@ [xmlns ,package-relationships-namespace])
            ,@(for/list ([sheet (in-list (workbook-sheets book))])
                (xml (Relationship
                      (@ [Id     ,(part-id sheet)]
                         [Type   ,worksheet-namespace]
                         [Target ,(path->string (part-path sheet #:relative-to xl-path))])))))))))

(define (worksheet-xml sheet)
  (xml ,standalone-header-xml
       (worksheet (@ [xmlns   ,spreadsheetml-namespace]
                     [xmlns:r ,workbook-namespace])
                  (sheetData ,@(for/list ([y (in-list (worksheet-y-indices sheet))])
                                 (xml (row (@ [r ,(y->row y)])
                                           ,@(for/list ([x (in-list (worksheet-x-indices sheet y))])
                                               (let ([cell (worksheet-ref sheet x y)])
                                                 (match (cell-value cell)
                                                   [(? number? n)  (xml (c (@ [r ,(xy->ref x y)])
                                                                           (v ,n)))]
                                                   [(? boolean? b) (xml (c (@ [r ,(xy->ref x y)])
                                                                           (v ,(if b "true" "false"))))]
                                                   [(? string? s)  (xml (c (@ [r ,(xy->ref x y)] [t "inlineStr"])
                                                                           (is (t ,s))))]
                                                   [(? formula? f) (xml (c (@ [r ,(xy->ref x y)])
                                                                           (f ,(formula->string sheet f))))]))))))))))

; Provide statements -----------------------------

(provide/contract
 [standalone-header-xml      xml?]
 [spreadsheetml-namespace    string?]
 [workbook-namespace         string?]
 [content-types-xml          (-> workbook? xml?)]
 [package-relationships-xml  (-> workbook? xml?)]
 [workbook-xml               (-> workbook? xml?)]
 [workbook-relationships-xml (-> workbook? xml?)]
 [worksheet-xml              (-> worksheet? xml?)])