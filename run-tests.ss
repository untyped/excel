#lang scheme/base

(require "test-base.ss")

(require "all-excel-tests.ss")

(print-struct #t)
(print-hash-table #t)

(run-tests all-excel-tests)