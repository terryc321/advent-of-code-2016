
#|

part 2 solution 
#(((node 0) (x 0) (y 0) (size 10) (used 8) (avail 2) (percent 80))
  ((node 1) (x 0) (y 1) (size 11) (used 6) (avail 5) (percent 54))
  ((node 2) (x 0) (y 2) (size 32) (used 28) (avail 4) (percent 87))
  ((node 3) (x 1) (y 0) (size 9) (used 7) (avail 2) (percent 77))
  ((node 4) (x 1) (y 1) (size 8) (used 0) (avail 8) (percent 0))
  ((node 5) (x 1) (y 2) (size 11) (used 7) (avail 4) (percent 63))
  ((node 6) (x 2) (y 0) (size 10) (used 6) (avail 4) (percent 60))
  ((node 7) (x 2) (y 1) (size 9) (used 8) (avail 1) (percent 88))
  ((node 8) (x 2) (y 2) (size 9) (used 6) (avail 3) (percent 66)))

|#

;; since this is the main module dont need a module at top level
;; (define-module (my main)
;;   #:exports ())

;; now load my definitions 
(load "grid.scm")
(load "bar.scm")

(import (ice-9 format))
(import (ice-9 pretty-print))
(import (ice-9 match))

(define pp pretty-print)


;; (import (my grid))
;;  (import (my main))

(format #t "input => ~a ~%" input)
(format #t "grid => ~a ~%" (make-grid 3 3 ))

;; this generate a starting point for small grid
;; (nodes->grid input 3 3)
;; (nodes->grid input 36 25) -- for larger puzzle
(define nodes->grid
  (lambda (ys sx sy)
    (let ((grid (make-grid sx sy)))
      (letrec ((foo (lambda (xs)
		      (cond
		       ((null? xs) #f)
		       (#t (let ((node (car xs)))
			     (match node
			       ((_ ('x x) ('y y) ('size size) ('used used) _ _)
				(let ((x (+ x 1))
				      (y (+ y 1)))
				  (grid-xy! grid x y `(used ,used size ,size))
				  (foo (cdr xs)))))))))))
	(foo (vector->list ys))
	grid))))

;; 1 to 36 on x axis , 1 to 25 on y axis
(define lisp->scala
  (lambda ()
    (let* ((wid 36)
	   (hgt 25)
	   (grid (nodes->grid input wid hgt)))
      (letrec ((foo (lambda (i j)
		      (cond
		       ((> j hgt) #f)
		       ((> i wid) (foo 1 (+ j 1)))
		       (#t (let ((elem (grid-xy grid i j)))
			     (match elem
			       (('used used 'size size)
				(let ((gold (if (and (= i wid)(= j 1)) 1 0)))
				  (format #t "g.set(~a,~a,(~a,~a,~a));~%"
					  i j used size gold)))
			       ( _ (error "no match")))
			     (foo (+ i 1) j)))))))
	(foo 1 1)))))
				


#|
 some sort of search to see to get (3,3) to (1,1)
 keep track of where G is

 grid move  
 new grid + the move from (x y) -> to (x2 y2)
 allow us to track G  if Gx = x and Gy = y  then G ' = (x2,y2)
|#
(define search
  (lambda ()
    (let* ((sx 3)
	   (sy 3)
	   (start-grid (nodes->grid input sx sy)))
      (letrec ((foo (lambda (grid gx gy n hist)
		      ;;(format #t "foo ~a ~a ~%" gx gy)
		      ;;(show-grid grid) ;; why alway same ??		      
		      (cond
		       ((and (= gx 1)(= gy 1)) (format #t "solution in ~a steps ~%" n))
		       (#t
			;; find all possible grid moves
			(for-each-possible-moves grid sx sy hist
						 (lambda (copy from-x from-y to-x to-y)
						   ;;(format #t " AHA ~a ~a -> ~a ~a ~%" from-x from-y to-x to-y)
						   ;;(show-grid copy)
						   (cond
						    ;; if this is the G node we moved
						    ;; update foo routine reflect that
						    ((and (= gx from-x)
							  (= gy from-y))
						     (foo grid
							  to-x
							  to-y
							  (+ n 1)
							  (cons copy hist)))
						    (#t
						     ;; otherwise G stays where it was
						     (foo grid gx gy (+ n 1)
							  (cons copy hist))))))
			)))))
	(let ((n-steps 0)
	      (history '())
	      (goal-x 3)
	      (goal-y 1))
	  (foo start-grid goal-x goal-y n-steps history))))))



(define (up grid x y hist func used size)
  (let ((node2 (grid-xy grid x (- y 1)))) ;; up negative Y
    (match node2
      (('used used2 'size size2)
       (let ((avail2 (- size2 used2)))
	 (cond
	  ((>= avail2 used)
	   (let ((copy (grid-copy grid)))
	     (grid-xy! copy x (- y 1)  `(used ,(+ used used2) size ,size2))
	     (grid-xy! copy x y `(used 0 size ,size))
	     (when (not (member copy hist))
	       (func copy x y x (- y 1)))))))))))

(define (down grid x y hist func used size)
  (let ((node2 (grid-xy grid x (+ y 1)))) 
    (match node2
      (('used used2 'size size2)
       (let ((avail2 (- size2 used2)))
	 (cond
	  ((>= avail2 used)
	   (let ((copy (grid-copy grid)))
	     (grid-xy! copy x (+ y 1)  `(used ,(+ used used2) size ,size2))
	     (grid-xy! copy x y `(used 0 size ,size))
	     (when (not (member copy hist))
	       (func copy x y x (- y 1)))))))))))

(define (left grid x y hist func used size)
  (let ((node2 (grid-xy grid (- x 1) y))) ;; left negative X
    (match node2
      (('used used2 'size size2)
       (let ((avail2 (- size2 used2)))
	 (cond
	  ((>= avail2 used)
	   (let ((copy (grid-copy grid)))
	     (grid-xy! copy (- x 1) y `(used ,(+ used used2) size ,size2))
	     (grid-xy! copy x y `(used 0 size ,size))
	     (when (not (member copy hist))
	       (func copy x y x (- y 1)))))))))))

  
(define (right grid x y hist func used size)
  (let ((node2 (grid-xy grid (+ x 1) y))) ;; right positive X 
    (match node2
      (('used used2 'size size2)
       (let ((avail2 (- size2 used2)))
	 (cond
	  ((>= avail2 used)
	   (let ((copy (grid-copy grid)))
	     (grid-xy! copy (+ x 1) y `(used ,(+ used used2) size ,size2))
	     (grid-xy! copy x y `(used 0 size ,size))
	     (when (not (member copy hist))						     
	       (func copy x y x (- y 1)))))))))))


;; func GRID from-x from-y to-x to-y
(define for-each-possible-moves
  (lambda (grid sx sy hist func)
    (letrec ((foo (lambda (x y)
		    ;;(format #t "foreach ~a ~a ~%" x y)
		    (cond
		     ((> y sy) #f)
		     ((> x sx) (foo 1 (+ y 1)))
		     (#t
		      (let ((node (grid-xy grid x y)))
			(match node
			  (('used used 'size size)
			   (cond
			    ((> used 0) ;; not moving hole around not point
			   (when (> y 1) (up grid x y hist func used size))
			   (when (< y sy) (down grid x y hist func used size))
			   (when (> x 1) (left grid x y hist func used size))
			   (when (< x sx) (right grid x y hist func used size)))))
			  ( _ (format #t "no match on expr => ~a ~%" node)))
			(foo (+ x 1) y)))))))
      (foo 1 1))))


(define debug
  (lambda ()
    (let* (
	  (sx 3)
	  (sy 3)
	  (start (nodes->grid input sx sy))
	  (hist '()))
    (for-each-possible-moves start sx sy hist
			     (lambda (copy from-x from-y to-x to-y)
			       (format #t "~%---------------------------------------------~%")
			       (show-grid start)
			       (format #t "~%")
			       (format #t "~%[ moved ~a ~a => ~a ~a ]~%"
				       from-x from-y to-x to-y)
			       (show-grid copy)
			       (format #t "~%")))
    )))

(define search2
  (lambda (max-depth)
    (let* ((sx 3)
	   (sy 3)
	   (start-grid (nodes->grid input sx sy)))
      (letrec ((foo (lambda (grid gx gy n hist)
		      ;; (show-grid grid)
		      (cond
		       ((and (= gx 1)(= gy 1))
			(format #t "SOLUTION EXISTS at ~a STEPS ~%" n))
		       ((> n max-depth) #f)
		       (#t
			(for-each-possible-moves grid sx sy hist
						 (lambda (copy from-x from-y to-x to-y)
						   ;;(format #t " AHA ~a ~a -> ~a ~a ~%" from-x from-y to-x to-y)
						   ;;(show-grid copy)
						   (cond
						    ;; if this is the G node we moved
						    ;; update foo routine reflect that
						    ((and (= gx from-x)
							  (= gy from-y))
						     (foo copy
							  to-x
							  to-y
							  (+ n 1)
							  (cons copy hist)))
						    (#t
						     ;; otherwise G stays where it was
						     (foo copy gx gy (+ n 1)
							  (cons copy hist)))))))))))
	(foo start-grid 3 1 0 '())))))

(define brute
  (lambda ()
    (letrec ((foo (lambda (i)
		    (format #t "trying solution lengths of ~a ~%" i)
		    (search2 i)
		    (foo (+ i 1)))))
      (foo 1))))







			






















