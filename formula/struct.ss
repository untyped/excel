#lang scheme/base

(require "../base.ss"
         "struct-internal.ss")

; Provide statements -----------------------------

(provide (except-out (all-from-out "struct-internal.ss")
                     make-literal)
         (rename-out [create-literal make-literal]))