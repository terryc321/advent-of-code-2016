
#|
chicken 5 
|#

(define input
  (with-input-from-file "input"
    (lambda ()
      (read))))

;; start at arbitrary point x y
(define x 0)
(define y 0)
(define heading 'north)

(define (left)
  (cond
   ((eq? heading 'north) (set! heading 'west))
   ((eq? heading 'west) (set! heading 'south))
   ((eq? heading 'south) (set! heading 'east))
   ((eq? heading 'east) (set! heading 'north))
   (#t (error "left" (list "heading not known n e s w")))))

(define (right)
  (cond
   ((eq? heading 'north) (set! heading 'east))
   ((eq? heading 'west) (set! heading 'north))
   ((eq? heading 'south) (set! heading 'west))
   ((eq? heading 'east) (set! heading 'south))
   (#t (error "right" (list "heading not known n e s w")))))

(define (forward n)
  (cond
   ((eq? heading 'north) (set! y (+ y n)))
   ((eq? heading 'west) (set! x (- x n)))
   ((eq? heading 'south) (set! y (- y n)))
   ((eq? heading 'east) (set! x (+ x n)))
   (#t (error "forward" (list "heading not known n e s w")))))

(define (reset)
  (set! x 0)
  (set! y 0)
  (set! heading 'north))


(define (solve)
  (reset)
  (letrec ((foo (lambda (xs)
		  (cond
		   ((null? xs) #f)
		   (#t (let ((dir (car xs))
			     (dist (car (cdr xs))))
			 (cond
			  ((eq? dir 'left) (left))
			  ((eq? dir 'right) (right))
			  (#t (error "solve : dir not known")))
			 (forward dist)
			 (foo (cdr (cdr xs)))))))))
    (foo input)))

  
#|
#;435> x
-140
#;436> y
138
#;437>

140 west and 138 north

#;442> (+ (abs x)(abs y))
278

278 blocks away

|#
   
   

