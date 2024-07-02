
;; guile does not support ~ home directory in pathname ??
;; (chdir "/home/terry/code/advent-of-code/advent-of-code-2016/day21/guile")
;; (getcwd)
;; (+ 1 2)

(import (ice-9 format))
(import (srfi srfi-1))
(import (ice-9 pretty-print))
(import (ice-9 match))

(define pp pretty-print)

(define demo (call-with-input-file "demo"
		(lambda (port)
		  (read port))))


;; start with abcde
;; ((swapxy 4 0)
;;  (swapletter #\d #\b)
;;  (rev 0 4)
;;  (rotleft 1)
;;  (move 1 4)
;;  (move 3 0)
;;  (rotpos #\b)
;;  (rotpos #\d))


;; if we define word as "abcde" this is an unchangeable string ??
(define word (make-string 5 #\a))
(define (swapxy x y)
  (let ((a (string-ref word x))
	(b (string-ref word y)))
    (string-set! word x b)
    (string-set! word y a)))

(define (demo)
  (set! word (make-string 5 #\a))
  (string-set! word 0 #\a)
  (string-set! word 1 #\b)
  (string-set! word 2 #\c)
  (string-set! word 3 #\d)
  (string-set! word 4 #\e)
  ;; we need to make a copy of string before we mutate it ??
  (set! word (string-copy "abcde")) 
  (swapxy 4 0)
  word)

(demo)


      
      
      
      
