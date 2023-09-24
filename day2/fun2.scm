
(import scheme)
(import (chicken format))

(import srfi-1)

(define input
  (with-input-from-file "input"
    (lambda ()
      (read))))


#|

     1           : y 5
  2  3  4        : y 4
5 6 *7* 8  9     : y 3
  A  B  C        : y 2
     D           : y 1
----------------------
1 2  3  4  5      : x

start *7*
     x=3 y=3

|#
(define bath `(
	                              ((3 5) 1)
	                  ((2 4) 2)   ((3 4) 3)  ((4 4) 4)
	       ((1 3) 5)  ((2 3) 6)   ((3 3) 7)  ((4 3) 8)   ((5 3) 9)
	                  ((2 2) A)   ((3 2) B)  ((4 2) C)
	                              ((3 1) D) 
				      ))


#|
up down left right

str->act : s -> [actions]
given single string fire off actions 

|#

(define x 3)
(define y 3)

(define (reset)
  (set! x 3)
  (set! y 3)
  #t)

;;(define (check-xy a b)
  
(define (up)
  (let ((xy (list x (+ y 1))))
    (cond
     ((assoc xy bath) (set! y (+ y 1)))
     (#t #f))))

(define (down)
  (let ((xy (list x (- y 1))))
    (cond
     ((assoc xy bath) (set! y (- y 1)))
     (#t #f))))

(define (left)
  (let ((xy (list (- x 1) y)))
    (cond
     ((assoc xy bath) (set! x (- x 1)))
     (#t #f))))

(define (right)
  (let ((xy (list (+ x 1) y)))
    (cond
     ((assoc xy bath) (set! x (+ x 1)))
     (#t #f))))


#|

keypad

1 2 3
4 5 6
7 8 9

|#

(define (xy->num)
  (let* ((xy (list x y))
	 (asc (assoc xy bath)))
    (cond
     (asc (second asc))
     (#t (error "xy->num" (list "x y combo not on keypad" x y))))))

(define (str->act s)
  (let ((len (string-length s))
	(i 0))
    (letrec ((foo (lambda (i)
		    (cond
		     ((>= i len) #f)
		     (#t
		      (let ((ch (string-ref s i)))
			(cond
			 ((char=? ch #\R) (right))
			 ((char=? ch #\L) (left))
			 ((char=? ch #\U) (up))
			 ((char=? ch #\D) (down))
			 (#t (error "str->act" (list "ch not recognised" ch))))
		      (foo (+ i 1))))))))
      (foo 0))))

(define (keypad)
  (reset)
  (letrec ((foo (lambda (xs)
		  (cond
		   ((null? xs) #f)
		   (#t (str->act (car xs))
		       (format #t "keypad ~a " (xy->num))
		       (foo (cdr xs)))))))
    (foo input)))

		      
#|

#;1290> (keypad)
keypad A keypad 6 keypad B keypad 3 keypad 5 #f

A 6 B 3 5


|#
