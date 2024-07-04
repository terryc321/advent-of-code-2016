


;; (define-module (my bar)
;;   #:export (input))

(import (ice-9 format))
(import (srfi srfi-1))
(import (ice-9 pretty-print))
(import (ice-9 match))
;;(define pp pretty-print)


;; parse/data.scm  ... full puzzle
;; parse/data2.scm ... small puzzle
(define input (call-with-input-file "parse/data.scm"
		(lambda (port)
		  (list->vector (read port)))))

