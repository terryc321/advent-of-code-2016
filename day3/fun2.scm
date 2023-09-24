
(import scheme)
(import (chicken format))
(import (chicken sort))

(import srfi-1)

(define input
  (with-input-from-file "input"
    (lambda ()
      (read))))




(define (read-values xs)
  (cond
   ((null? xs) #f)
   (#t (let ((a (list-ref xs 0))   (b (list-ref xs 1)) (c (list-ref xs 2))
	     (a2 (list-ref xs 3))   (b2 (list-ref xs 4)) (c2 (list-ref xs 5))
	     (a3 (list-ref xs 6))   (b3 (list-ref xs 7)) (c3 (list-ref xs 8)))
	 (is-triangle? a a2 a3)
	 (is-triangle? b b2 b3)
	 (is-triangle? c c2 c3)	 
	 (read-values (drop xs 9))))))



#|
The sum of the lengths of any two sides of a triangle
is always larger than the length of the third side 

|#


(define tot 0)

(define (is-triangle? a b c)
  (let ((tri (sort (list a b c) <)))
    (when
	(> (+ (first tri)
	      (second tri))
	   (third tri))
      (set! tot (+ tot 1)))))

(define (solve)
  (set! tot 0)
  (read-values input)
  tot)

#|

869 triangles

part I I 
1544 triangles read data in columns


|#



