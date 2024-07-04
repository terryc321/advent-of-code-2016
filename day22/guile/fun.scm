

(import (ice-9 format))
(import (srfi srfi-1))
(import (ice-9 pretty-print))
(import (ice-9 match))

(define pp pretty-print)

;; make it a vector
(define input (call-with-input-file "parse/data.scm"
		(lambda (port)
		  (list->vector (read port)))))

#|
(((node 0) (x 0) (y 0) (size 94) (used 67) (avail 27) (percent 71))
...
.... more nodes ...
 ...
((node 899) (x 35) (y 24) (size 85) (used 67) (avail 18) (percent 78)))

 (* 36 25)
= 900

36 wide by 25 high
? four nodes directly adjacent to it
up / down / left / right mean ?

  o
o X o
at X edge only three neighbours

o
X o 
at X corner only two neighbours

any two pair of nodes , not same node ,
node A not empty
node B not node A
node A used fit in node B avail

|#

(define iter
  (lambda (vec)
    (let ((lim (- (vector-length vec) 1))
	  (compares 0)
	  (viable 0))
      (letrec ((foo (lambda (i j)
		      (cond
		       ((> j lim) #f)
		       ((> i lim) (foo 0 (+ j 1)))
		       ((= i j) (foo (+ i 1) j))
		       (#t (let ((node-a (vector-ref vec i))
				 (node-b (vector-ref vec j)))
			     ;;
			     (let ((used-a (second (assoc 'used node-a)))
				   (avail-b (second (assoc 'avail node-b))))
			       (when
				   (and (> used-a 0) (<= used-a avail-b))
				 (set! viable (+ 1 viable)))
			     ;; record comparisons
			     (set! compares (+ 1 compares))
			     ;;(format #t "~a ~a ~%" node-a node-b)
			     (foo (+ i 1) j))))))))
	       (foo 0 0)
	       (format #t "there are ~a viable pairs ~%" viable)
	       compares))))

#|

scheme@(guile-user)> ,t (iter input)
there are 864 viable pairs 

......... ACCEPTED ..........

|#




		       











  
