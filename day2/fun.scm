
(import scheme)
(import (chicken format))

(define input
  (with-input-from-file "input"
    (lambda ()
      (read))))

#|
up down left right

str->act : s -> [actions]
given single string fire off actions 

|#

(define x 0)
(define y 0)

(define (check-xy a b)
  (cond
   ((and (< a 2)(> a -2)
	 (< b 2)(> b -2))
    #t)
   (#t (error "check-xy" (list "xy out of bounds " a b)))))

(define (up)
  (cond
   ((>= y 1) #f)
   (#t (set! y (+ y 1))))
  (check-xy x y))


(define (down)
  (cond
   ((<= y -1) #f)
   (#t (set! y (- y 1))))
  (check-xy x y))

(define (left)
  (cond
   ((<= x -1) #f)
   (#t (set! x (- x 1))))
  (check-xy x y))

(define (right)
  (cond
   ((>= x 1) #f)
   (#t (set! x (+ x 1))))
  (check-xy x y))


#|

keypad

1 2 3
4 5 6
7 8 9

|#

(define (xy->num)
  (cond
   ((and (= x -1)(= y 1))  1)
   ((and (= x  0)(= y 1))  2)
   ((and (= x  1)(= y 1))  3)
   ;; middle
   ((and (= x -1)(= y 0)) 4)
   ((and (= x 0)(= y 0)) 5)
   ((and (= x 1)(= y 0)) 6)
   ;; lower    << had bug :: y 1 for 7,8,9 >>
   ((and (= x -1)(= y -1)) 7)
   ((and (= x 0)(= y -1)) 8)
   ((and (= x 1)(= y -1)) 9)
   (#t (error "xy->num" (list "x y combo not on keypad" x y)))))



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

(define (reset)
  (set! x 0)
  (set! y 0)
  #t)

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

keypad 1 keypad 4 keypad 3 keypad 8 keypad 7 #f

14387

wrong ? had keypad coordinates upside down

keypad 7 keypad 4 keypad 9 keypad 2 keypad 1 #f

74921     ......... accepted

|#
