
(import scheme)
(import (chicken format))
(import (chicken sort))

(import procedural-macros)
(import regex)

;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))

(import sequences)

(import srfi-1)

(define input
  (with-input-from-file "day9/input"
    (lambda ()
      (read))))


