
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
  (with-input-from-file "day6/input"
    (lambda ()
      (read))))

(define alphabet #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))


(define arr0 #f)
(define arr1 #f)
(define arr2 #f)
(define arr3 #f)
(define arr4 #f)
(define arr5 #f)
(define arr6 #f)
(define arr7 #f)
(define arr8 #f)


(define (count-em)
  (set! arr0 (make-vector 26 0))
  (set! arr1 (make-vector 26 0))
  (set! arr2 (make-vector 26 0))
  (set! arr3 (make-vector 26 0))
  (set! arr4 (make-vector 26 0))
  (set! arr5 (make-vector 26 0))
  (set! arr6 (make-vector 26 0))
  (set! arr7 (make-vector 26 0))
  
  (letrec (
	   (word (lambda (str)
		   (let ((n (- (char->integer (string-ref str 0)) 97)))
		     (vector-set! arr0 n (+ 1 (vector-ref arr0 n))))

		   (let ((n (- (char->integer (string-ref str 1)) 97)))
		     (vector-set! arr1 n (+ 1 (vector-ref arr1 n))))
		   
		   (let ((n (- (char->integer (string-ref str 2)) 97)))
		     (vector-set! arr2 n (+ 1 (vector-ref arr2 n))))

		   (let ((n (- (char->integer (string-ref str 3)) 97)))
		     (vector-set! arr3 n (+ 1 (vector-ref arr3 n))))

		   (let ((n (- (char->integer (string-ref str 4)) 97)))
		     (vector-set! arr4 n (+ 1 (vector-ref arr4 n))))

		   (let ((n (- (char->integer (string-ref str 5)) 97)))
		     (vector-set! arr5 n (+ 1 (vector-ref arr5 n))))

		   (let ((n (- (char->integer (string-ref str 6)) 97)))
		     (vector-set! arr6 n (+ 1 (vector-ref arr6 n))))

		   (let ((n (- (char->integer (string-ref str 7)) 97)))
		     (vector-set! arr7 n (+ 1 (vector-ref arr7 n))))
		   ))
	   
	   (loop (lambda (ys)
		   (cond
		    ((null? ys) #f)
		    (#t
		     (word (car ys))
		     (loop (cdr ys)))))))
    (loop input)))


(define (arr-ch arr)
  (let ((max-i 0)
	(max-n -999)
	(max-ch #f))    
    (letrec ((loop (lambda (i)
		     ;;(format #t "i = ~a ~%" i)
		     (cond
		      ((>= i 26) #f)
		      (#t
		       (let ((v (vector-ref arr i)))
			 ;;(format #t " v= ~a ~%" v)
			 (when (> v max-n)
			   (set! max-n v)
			   (set! max-ch (vector-ref alphabet i)))
			 (loop (+ i 1))))))))
      (loop 0)
      max-ch)))



(define (solve)
  (count-em)
  (format #f "~a~a~a~a~a~a~a~a"
	  (arr-ch arr0)
	  (arr-ch arr1)
	  (arr-ch arr2)
	  (arr-ch arr3)
	  (arr-ch arr4)
	  (arr-ch arr5)
	  (arr-ch arr6)
	  (arr-ch arr7)))


#|

#;654> (solve)
"bjosfbce"

|#



