
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
   (#t (let ((a (first xs))
	     (b (second xs))
	     (c (third xs)))
	 (is-triangle? a b c)
	 (read-values (drop xs 3))))))




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

|#



