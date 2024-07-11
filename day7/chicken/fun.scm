
(import scheme)
(import (chicken format))
(import (chicken sort))

(import procedural-macros)

;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))


(import srfi-1)

(define input
  (with-input-from-file "input"
    (lambda ()
      (read))))

(define alphabet #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k
		   #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
		   #\w #\x #\y #\z))


#|
iterate over string
if [ ]

|#
(define (foo str)
  (format #t "str >~a<~%" str)
  (call/cc (lambda (exit)
	     (let ((len (string-length str))
		   (ref-out #f)
		   (ref-in #f))
	       (letrec ((loop-on (lambda (from i)
				   (cond
				    ((>= i len)
				     (error "foo" (list "not closed hypertext [ .."))
				     ;;(format #t "ON : ***~a***~%" (substring str from i))
				     (when (refl (substring str from i))
				       (set! ref-in #t)
				       ;; escape know never going to satisfy
				       (exit #f)
				       )				     
				     #f)
				    ((char=? (string-ref str i) #\] )
				     ;;(format #t "ON : ***~a***~%" (substring str from i))
				     (when (refl (substring str from i))
				       (set! ref-in #t)
				       ;; escape know never going to satisfy
				       ;;(exit #f)
				       )				     
				     (loop-off (+ i 1) (+ i 1)))
				    (#t
				     (loop-on from (+ i 1))))))
			(loop-off (lambda (from i)
				    (cond
				     ((>= i len)
				      ;; <<< was bug : missed when string ends still some OUT >>
				      ;;(format #t "OFF : ***~a***~%" (substring str from i))
				      (when (refl (substring str from i))
					(set! ref-out #t))
				      #f)
				     ((char=? (string-ref str i) #\[ )
				      ;;(format #t "OFF : ***~a***~%" (substring str from i))
				      (when (refl (substring str from i))
					(set! ref-out #t))
				      (loop-on (+ i 1) (+ i 1)))
				     (#t
				      (loop-off from (+ i 1)))))))
		 (loop-off 0 0)
		 (and ref-out (not ref-in)))))))





#|
two different letters then reflected
a b b a
|#
(define (refl str)
  (call/cc (lambda (exit)
	     (let ((len (string-length str)))
	       (letrec ((loop (lambda (i)
				(cond
				 ((>= i (- len 3)) #f)
				 (#t
				  (let ((ch (string-ref str i))
					(ch2 (string-ref str (+ i 1)))
					(ch3 (string-ref str (+ i 2)))
					(ch4 (string-ref str (+ i 3))))
				    ;;(format #t "chars = ~a ~a ~a ~a ~%" ch ch2 ch3 ch4)
				    (when (and (not (char=? ch ch2))
					       (char=? ch ch4)
					       (char=? ch2 ch3))
				      (exit #t))			       
				    (loop (+ i 1))))))))
		 (loop 0)))
	     #f)))

(define (solve)
  (let ((n 0))
    (letrec ((loop (lambda (ys)
		     (cond
		      ((null? ys) #f)
		      (#t
		       (when (foo (car ys))
			 (set! n (+ n 1)))
		       (loop (cdr ys)))))))
      (loop input)
      n)))


#|

with call/cc exit
0.373s CPU time, 181987/6922 mutations (total/tracked), 0/3408 GCs (major/minor), maximum live heap: 1.26 MiB
118

without call/cc
0.576s CPU time, 184698/7054 mutations (total/tracked), 0/3468 GCs (major/minor), maximum live heap: 1.26 MiB
118



|#






    
