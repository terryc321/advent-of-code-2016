
(import (ice-9 format))
(import (srfi srfi-1))
(import (ice-9 pretty-print))
(import (ice-9 match))

(define pp pretty-print)

(define s>c
  (lambda (s)
    (string-ref (symbol->string s) 0)))

(define tidy
  (let ((nodecount -1))
    (lambda (expr)
      (match expr
	(('line ('nodex '/dev/grid/node-x)
		x
		_ 
		y
		('size size _)
		('used used _)
		('avail avail _)
		('percent percent _))
	 (set! nodecount (+ 1 nodecount))
	 `((node ,nodecount) (x ,x) (y ,y) (size ,size) (used ,used) (avail ,avail)
	   (percent ,percent)))
	( _ (format #t ">>>>>>>~%")
	  (format #t "unrecognised [~a] ~%" expr)
	  '())))))

  

;; top of parse is r "root" node
;; (root <1> <2> <3> ...)
;; reason for cdr to get (<1> <2> <3> ...)
(define tidy-parse
  (lambda (filename)
    (map tidy (cdr (call-with-input-file filename
		     (lambda (port)
		       (read port)))))))

(define input (tidy-parse "../../output"))
(define demo (tidy-parse "../../output2"))



