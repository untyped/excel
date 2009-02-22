#lang scheme/base

(require "base.ss")

(require "xml-cache.ss"
         "path.ss"
         "ref.ss"
         "struct.ss"
         "xml-internal.ss"
         "xml-style.ss"
         "xml-worksheet.ss"
         "formula/formula.ss")

; Content ----------------------------------------

; workbook -> xml
(define (content-types-xml book)
  (xml ,standalone-header-xml
       (Types (@ [xmlns ,content-types-namespace])
              (Default  (@ [Extension "rels"]
                           [ContentType ,package-relationships-content-type]))
              (Override (@ [PartName "/xl/styles.xml"]
                           [ContentType ,stylesheet-content-type]))
              ,@(for/list ([part (in-list (cons book (workbook-sheets book)))])
                  (xml (Override (@ [PartName     ,(path->string (package-part-path part #:absolute? #t))]
                                    [ContentType  ,(match part
                                                     [(? workbook?)  workbook-content-type]
                                                     [(? worksheet?) worksheet-content-type])])))))))

; workbook -> xml
(define (package-relationships-xml book)
  (xml ,standalone-header-xml
       (Relationships (@ [xmlns ,package-relationships-namespace])
                      (Relationship (@ [Id     ,(package-part-id book)]
                                       [Type   ,office-document-relationship]
                                       [Target ,(path->string (package-part-path book))])))))

; workbook -> xml
(define (workbook-xml book)
  (xml ,standalone-header-xml
       (workbook (@ [xmlns   ,spreadsheetml-namespace]
                    [xmlns:r ,workbook-namespace])
                 (sheets ,@(for/list ([sheet (in-list (workbook-sheets book))]
                                      [index (in-naturals 1)])
                             (xml (sheet (@ [name    ,(worksheet-name sheet)]
                                            [sheetId ,index]
                                            [r:id    ,(package-part-id sheet)]))))))))

; workbook -> xml
(define workbook-relationships-xml
  (let ([xl-path (build-path "xl")])
    (lambda (book)
      (xml ,standalone-header-xml
           (Relationships
            (@ [xmlns ,package-relationships-namespace])
            (Relationship (@ [Id     ,stylesheet-part-id]
                             [Type   ,stylesheet-namespace]
                             [Target "styles.xml"]))
            ,@(for/list ([sheet (in-list (workbook-sheets book))])
                (xml (Relationship
                      (@ [Id     ,(package-part-id sheet)]
                         [Type   ,worksheet-namespace]
                         [Target ,(path->string (package-part-path sheet #:relative-to xl-path))])))))))))

; Provide statements -----------------------------

(provide stylesheet-xml!
         worksheet-xml)

(provide/contract
 [standalone-header-xml      xml?]
 [spreadsheetml-namespace    string?]
 [workbook-namespace         string?]
 [content-types-xml          (-> workbook? xml?)]
 [package-relationships-xml  (-> workbook? xml?)]
 [workbook-xml               (-> workbook? xml?)]
 [workbook-relationships-xml (-> workbook? xml?)])