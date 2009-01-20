#lang setup/infotab

(define name "Excel API")

(define blurb 
  '((p "Write Excel Open XML files (" (tt ".xslx") " extension) using Scheme.")))

(define release-notes
  '((p "Initial release.")))

(define primary-file
  "main.ss")

(define url "http://svn.untyped.com/excel/")

#;(define scribblings '(("scribblings/excel.scrbl" (multi-page))))

(define categories '(devtools ui xml))

(define required-core-version "4.1.3")

(define repositories '("4.x"))
