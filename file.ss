#lang scheme/base

(require "base.ss")

(require #;file/zip
         scheme/file
         scheme/string
         scheme/system
         (unlib-in file profile)
         "xml-cache.ss"
         "path.ss"
         "ref.ss"
         "struct.ss"
         "xml.ss")

(define-timer calc-styles-timer)
(define-timer calc-sheets-timer)

;  workbook
;  absolute-path 
;  [#:temp-dir absolute-path]
;  [#:exists (U 'error 'replace)]
; ->
;  void
(define (write-workbook
         book
         zip-path
         #:temp-dir [temp-dir (make-temp-dir-path)]
         #:exists   [exists   'error])
  (cond [(file-exists? zip-path)
         (match exists
           ['error   (error "file already exists" zip-path)]
           ['replace (delete-file zip-path)])]
        [(directory-exists? zip-path)
         (error "directory already exists" zip-path)]
        [(or (directory-exists? temp-dir) (file-exists? temp-dir))
         (error "temporary directory already exists" temp-dir)])
  
  (dynamic-wind
   void
   (lambda ()
     (let ([cache (make-cache)])
       (make-directory temp-dir)
       (parameterize ([current-directory temp-dir])
         (make-directory* (build-path "_rels"))
         (make-directory* (build-path "xl/_rels"))
         (make-directory* (build-path "xl/worksheets"))
         (let ([args (list* zip-path
                            (write-xml-file (content-types-path)               (content-types-xml book))
                            (write-xml-file (package-relationships-path)       (package-relationships-xml book))
                            (write-xml-file (package-part-path book)           (workbook-xml book))
                            (write-xml-file (workbook-relationships-path book) (workbook-relationships-xml book))
                            (write-xml-file (stylesheet-path book)             (profile calc-styles-timer stylesheet-xml! cache book))
                            (for/list ([sheet (in-list (workbook-sheets book))])
                              (write-xml-file (package-part-path sheet) (profile calc-sheets-timer worksheet-xml cache sheet))))])
           (unless (apply zip/system temp-dir args)
             (error (format "zip command failed: zip ~a" (string-join (map path->string args) " ")))))))
     (unless (file-exists? zip-path)
       (error "file was not created")))
   (lambda ()
     (when (directory-exists? temp-dir)
       (delete-directory/files temp-dir)))))

; Helpers ----------------------------------------

; path ... -> boolean
(define (zip/system dir . paths)
  (let/debug ([cmd-line (format "zip ~a" (string-join (map (cut format "~s" <>) (map path->string paths)) " "))])
             (system cmd-line)))

; -> path
(define (make-temp-dir-path)
  (make-non-conflicting-path (find-system-path 'temp-dir) "untyped-xl-temp"))

; path xml -> path
(define (write-xml-file path content)
  (write-text-file path (xml->string content)))

; path string -> path
(define (write-text-file path content)
  (with-output-to-file path
    (cut display content))
  path)

; Provide statements -----------------------------

(provide/contract
 [write-workbook (->* (workbook? (and/c path? absolute-path?))
                      (#:temp-dir (and/c path? absolute-path?) #:exists (or/c 'error 'replace))
                      void?)])