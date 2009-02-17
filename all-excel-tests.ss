#lang scheme/base

(require "test-base.ss")

(require "cache-test.ss"
         "range-test.ss"
         "ref-test.ss"
         "struct-style-test.ss"
         "struct-test.ss"
         "xml-fill-test.ss"
         "xml-font-test.ss"
         "xml-number-format-test.ss"
         "xml-style-test.ss"
         "xml-test.ss")

(define all-excel-tests
  (test-suite "excel"
    cache-tests
    range-tests
    ref-tests
    struct-style-tests
    struct-tests
    xml-fill-tests
    xml-font-tests
    xml-number-format-tests
    xml-style-tests
    xml-tests))

(provide all-excel-tests)