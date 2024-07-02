
(import (ice-9 format))
(import (srfi srfi-1))
(import (ice-9 pretty-print))
(import (ice-9 match))

(define pp pretty-print)

(define s>c
  (lambda (s)
    (string-ref (symbol->string s) 0)))

(define tidy
  (lambda (expr)
    (match expr
      (('statement ('rotposEx _ _ _ _ _ _ x)) `(rotpos ,(s>c x)))
      (('statement ('swapxyEx _ _ x _ _ y)) `(swapxy ,x ,y))
      (('statement ('swapletterEx _ _ x _ _ y)) `(swapletter ,(s>c x) ,(s>c y)))      
      (('statement ('moveEx _ _ x _ _ y)) `(move ,x ,y))
      (('statement ('rotrightEx _ _ x _)) `(rotright ,x))
      (('statement ('rotleftEx _ _ x _)) `(rotleft ,x))
      (('statement ('reverseEx _ _ x _ y)) `(rev ,x ,y))
      ( _a (format #t ">>>>>>>~%")
	(format #t "unrecognised ~a ~%" expr)
	(format #t ">>>>>>>~%")
	(error "unrecognised")))))


;; top of parse is r "root" node , reason for cdr
(define tidy-parse
  (lambda (filename)
    (map tidy (cdr (call-with-input-file filename
		     (lambda (port)
		       (read port)))))))

(define input (tidy-parse "input.scm"))
(define demo (tidy-parse "demo.scm"))






      
      
      
      
