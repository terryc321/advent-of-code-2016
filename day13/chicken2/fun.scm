


(import scheme)
(import (chicken format))
(import (chicken sort))
(import (chicken pretty-print))
(import (chicken irregex))
(import srfi-69)

;;(import (srfi srfi-115))
(import bindings)
(import procedural-macros)
;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))
(import sequences)
(import srfi-1)
(import simple-loops)
(define input 1352)

(define bin-n
  (lambda (n)
    (let* ((str (format #f "~b" n))
	   (c 0)
	   (len (string-length str)))
      ;; (format #t "binary of ~A is [~a]~%" n str)
      (do-for (i 0 len)
	      (let ((ch (string-ref str i)))
		(when (char=? ch #\1)
		  (set! c (+ c 1)))
		;; (format #t "i = ~a ~%" i)
		))
      (even? c))))

(define f1
  (lambda (x y)
    (+ (* x x) (* 3 x) (* 2 x y) y (* y y))))

;; f0 -> false is a wall
;;    -> true is open
(define f0
  (let ((hash (make-hash-table #:test equal?)))
    (lambda (x y)
      (let ((in (hash-table-ref/default hash (list x y) #f)))
	(cond
	 (in in)
	 (#t (let ((out (bin-n (+ (f1 x y) input))))
	       (cond
		(out (hash-table-set! hash (list x y) 'open)
		     'open)
		(#t  (hash-table-set! hash (list x y) 'wall)
		     'wall)))))))))

(define open?
  (lambda (x y)
    (eq? (f0 x y) 'open)))


(define remove-duplicate-append
  (lambda (xs ys)
    (cond
     ((null? xs) ys)
     ((member (car xs) ys) (remove-duplicate-append (cdr xs) ys))
     (#t (remove-duplicate-append (cdr xs) (cons (car xs) ys))))))

;;(remove-duplicates '(1 4 5 6 2 3 3 2 3 4 5 6) '(2 3))

(define flag74 #f)
(define flag3139 #f)

;; think its breadth first search
(define reach
  (lambda (pool been depth maxdepth)

    ;; when does 7 4 make an appearance ?
    (when (and (not flag74) (member '(7 4) pool))
      (format #t "square 7 4 at depth ~a ~%" depth)
      (set! flag74 #t))

    ;; when does 31 39 make an appearance ?
    (when (and (not flag3139) (member '(31 39) pool))
      (format #t "square 31 39 at depth ~a ~%" depth)
      (set! flag3139 #t))
    
    (let ((next '()))
      (letrec ((onboard-and-open?
		(lambda (p) (bind (x y) p (and (>= x 0)(>= y 0)(open? x y)))))
	       (add (lambda (p) (when (and (onboard-and-open? p)
					   (not (member p been))
					   (not (member p next)))
				  (set! next (cons p next))))))
	(cond
	 ((< depth maxdepth)
	  (do-list (p pool)
		   (bind (x y) p
			 (let ((up (list x (- y 1)))
			       (down (list x (+ y 1)))
			       (left (list (- x 1) y))
			       (right (list (+ x 1) y)))			   
			   (add up)
			   (add down)
			   (add left)
			   (add right))))
	  (reach next (remove-duplicate-append pool been) (+ depth 1) maxdepth))
	 (#t (remove-duplicate-append pool been)))))))

	  


(define test0
  (lambda ()
    (let ((pool `((1 1)))
	  (been '())
	  (depth 0)
	  (maxdepth 0))
      (reach pool been depth maxdepth))))

(define test1
  (lambda ()
    (let ((pool `((1 1)))
	  (been '())
	  (depth 0)
	  (maxdepth 1))
      (reach pool been depth maxdepth))))

(define test50
  (lambda ()
    (let ((pool `((1 1)))
	  (been '())
	  (depth 0)
	  (maxdepth 50))
      (let ((out (reach pool been depth maxdepth)))
	(remove-duplicate-append out '())))))

(define test3139
  (lambda ()
    (let ((pool `((1 1)))
	  (been '())
	  (depth 0)
	  (maxdepth 500))
      (let ((out (reach pool been depth maxdepth)))
	(remove-duplicate-append out '())))))



;; (length (test50)) ;;  1326 ... nope

(format #t "length of test50 => ~a ~%" (length (test50)))
(test3139)

;; threw me off as start on grid at 1,1
;; doing a breadth first search but ignored if square was a wall , just carried on
;; included open? to mean at x y if that square is open to move around in

#|
terry@debian:~/code/advent-of-code/advent-of-code-2016/day13/chicken2$

> csc fun.scm && time ./fun
length of test50 => 135 
square 31 39 at depth 90 

real	0m0.017s
user	0m0.011s
sys	0m0.006s

|#
			 
      
    
    
      
