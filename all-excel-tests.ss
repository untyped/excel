#lang scheme/base

(require "test-base.ss")

(require "cache-test.ss"
         "range-test.ss"
         "ref-test.ss"
         "struct-style-test.ss"
         "struct-test.ss"
         "swatch-test.ss"
         "xml-style-test.ss"
         "xml-test.ss")

(define all-excel-tests
  (test-suite "excel"
    cache-tests
    range-tests
    ref-tests
    struct-style-tests
    struct-tests
    xml-style-tests
    xml-tests
    swatch-tests))

(provide all-excel-tests)