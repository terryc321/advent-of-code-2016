
(import (chicken format)) ;; format #t "hello world"
(import (chicken pretty-print)) ;; pp 
(import (chicken process-context)) ;; current-directory
(import srfi-1) ;; first second third ...
(import srfi-69) ;; hash tables
(import bindings) ;; bind

#|
typical 2d grid input.txt

|#
(define reader
  (lambda ()
    (let ((hash (make-hash-table #:test equal?))
	  (width 0)
	  (height 0)
	  (people 0))
    (with-input-from-file "../input.txt"
      (lambda ()
	(letrec ((read-line (lambda (y)
			      (let ((line '()))
				(letrec ((foo (lambda (x)
						(let ((ch (read-char)))
						  (cond
						   ;; return eof 
						   ((and (null? line) (eof-object? ch)) ch)
						   ;; return line 
						   ((or (char=? ch #\newline)
							(eof-object? ch))
						    (list->string (reverse line)))
						   ;; keep slurp
						   (#t (set! line (cons ch line))
						       ;; record max width
						       (when (> x width) (set! width x))
						       ;; dump read to stout
						       (cond
							((char=? ch #\#)
							 (hash-table-set! hash (list x y) 'wall)
							 (format #t "wall at ~a,~a ~%" x y))
							((char=? ch #\.)
							 (hash-table-set! hash (list x y) 'clear)
							 (format #t "void at ~a,~a ~%" x y))
							((member ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
							 (let ((char-value (- (char->integer ch) (char->integer #\0))))
							   (format #t "value ~a at ~a,~a ~%" char-value x y)
							   ;; record value integer as position (x,y) in hash
							   (hash-table-set! hash char-value (list x y))
							   ;; also this position is also clear - ie not a wall
							   (hash-table-set! hash (list x y) 'clear)
							   ;; record start position
							   (when (= char-value 0)
							     (hash-table-set! hash 'start (list x y)))
							   (when (> char-value people)
							     (set! people char-value))
							   )))
						       ;; next char in line
						       (foo (+ x 1))))))))
				  (foo 1)))))
		 (read-all-lines (lambda ()
				   (let ((lines '()))					 
				     (letrec ((foo (lambda (y)
						     (let ((line (read-line y)))
						       (cond
							;; no more lines
							((eof-object? line) (reverse lines))
							(#t (set! lines (cons line lines))
							    ;; record max height
							    (when (> y height) (set! height y))							    
							    (foo (+ y 1))))))))
				       (foo 1))))))
	  (read-all-lines)
	  (hash-table-set! hash 'size (list width height))
	  (hash-table-set! hash 'width width)
	  (hash-table-set! hash 'height height)
	  (hash-table-set! hash 'people people)	  
	  hash
	  ))))))

(define input (reader))

(define copy-hash
  (lambda (h)
    (let ((copy (make-hash-table #:test equal?)))
      (hash-table-for-each h (lambda (k v)			       
			       (hash-table-set! copy k v)))
      copy)))

;; given a hash and two locations a , b
;; find shortest path by iteratively spreading out from any 
;; destructively modify h2 iteratively until we find or do nt find our location
;; entirely possible a location is not accessible (walled garden)

;; list ((a))
;; find all reachable from this



;; (define shortest-path
;;   (lambda (h a b)
;;     (let ((h2 (copy-hash h))
;; 	  (width (hash-table-ref h2 'width))
;; 	  (height (hash-table-ref h2 'height)))
;;       (call/cc (lambda (exit)
;; 		 (bind (ax ay) a
;; 		       (bind (bx by) b
;; 			     (letrec ((up (lambda (n x y)
;; 					    (let ((at (hash-table-ref/default h2 (list x (- y 1)) 'wall)))
;; 					      (when (eq? at 'clear)
;; 						#t))))
;; 				      (foo (lambda (n x y)
;; 					     (cond
;; 					      ((> y height) #f)
;; 					      ((> x width) (foo 1 (+ y 1)))
;; 					      (#t
;; 					       (up? n x y)
;; 					       (left? n x y)
;; 					       (down? n x y)
;; 					       (right? n x y)
;; 					       (foo n (+ x 1) y))))))
;; 			       (foo n ax ay)))))))))


(define clear?
  (lambda (x y)
    (let ((sq (hash-table-ref/default input (list x y) 'wall)))
      (eq? sq 'clear))))


;; find reachable takes a single position only
(define find-reachable
  (lambda (pos)
    (let ((reach '()))
      (bind (x y) pos	    
	    (when (clear? x (- y 1)) (set! reach (cons (list x (- y 1))  reach)))
	    (when (clear? x (+ y 1)) (set! reach (cons (list x (+ y 1))  reach)))
	    (when (clear? (- x 1) y) (set! reach (cons (list (- x 1) y)  reach)))
	    (when (clear? (+ x 1) y) (set! reach (cons (list (+ x 1) y)  reach)))
	    reach))))

(define remove-duplicates
  (lambda (xs)
    (cond
     ((null? xs) xs)
     ((null? (cdr xs)) xs)
     ((member (car xs) (cdr xs)) (remove-duplicates (cdr xs)))
     (#t (cons (car xs) (remove-duplicates (cdr xs)))))))

(define reachable-helper
  (lambda (target working-set depth visited-set)
    (cond
     ((member target working-set) depth)
     (#t
      ;;(format #t "working set ~a~%" working-set)
      (let ((next-gen (remove-duplicates (apply append (map find-reachable working-set)))))
	(let ((next-gen2 (filter (lambda (x) (not (member x visited-set))) next-gen)))
	  (reachable-helper target next-gen2 (+ depth 1) (append next-gen2 visited-set))
	  ))))))

;; assuming start and target are not same location - trivial
(define reachable
  (lambda (start target)
    (let ((working-set (list start))
	  (depth 0)
	  (visited-set (list start)))
      (reachable-helper target working-set depth visited-set))))

(define distance-ab
  (lambda (i j)
    (let ((pos (hash-table-ref input i))
	  (pos2 (hash-table-ref input j)))
      (reachable pos pos2))))

(define hashed-distance-ab
  (let ((hash (make-hash-table #:test equal?)))
    (lambda (i j)
      (let ((alpha (hash-table-ref/default hash (list i j) #f)))
	(cond
	 (alpha alpha)
	 (#t
	  (let ((dist (distance-ab i j)))
	    (hash-table-set! hash (list i j) dist)
	    (hash-table-set! hash (list j i) dist)
	    dist)))))))

(define chart-distances
  (lambda ()
    (letrec ((foo (lambda (x y)
		    (cond
		     ((and (> y 7) (> (+ x 2) 7)) #f)
		     ((> y 7) (foo (+ x 1) (+ x 2)))
		     ((= x y) (foo x (+ y 1)))
		     (#t
		      (let ((alpha (distance-ab x y))
			    (beta (hashed-distance-ab x y)))
			(when (not (= alpha beta))
			  (error "hash inconsistent !"))
			(format #t "~a <-> ~a : ~a : ~a ~%" x y alpha beta)
			(foo x (+ y 1))))))))
      (foo 0 0))))

(define chart-hashed-distances
  (lambda ()
    (letrec ((foo (lambda (x y)
		    (cond
		     ((and (> y 7) (> (+ x 2) 7)) #f)
		     ((> y 7) (foo (+ x 1) (+ x 2)))
		     ((= x y) (foo x (+ y 1)))
		     (#t
		      (let ((beta (hashed-distance-ab x y)))
			(format #t "~a <-> ~a : ~a ~%" x y beta)
			(foo x (+ y 1))))))))
      (foo 0 0))))





#|

algorithm to find reachable squares
breadth first approach 

start = where we start from
target = where want to reach

working set= (start)    depth 0     history?     visited = (start) also
-------------------------------------------------------------------------
find all places we can reach from working set

(apply append (map find-reachable working-set))


> (chart-distances)
0 <-> 1 : 50 
0 <-> 2 : 232 
0 <-> 3 : 164 
0 <-> 4 : 52 
0 <-> 5 : 230 
0 <-> 6 : 250 
0 <-> 7 : 46 
1 <-> 2 : 270 
1 <-> 3 : 202 
1 <-> 4 : 62 
1 <-> 5 : 268 
1 <-> 6 : 288 
1 <-> 7 : 84 
2 <-> 3 : 72 
2 <-> 4 : 276 
2 <-> 5 : 34 
2 <-> 6 : 22 
2 <-> 7 : 218 
3 <-> 4 : 208 
3 <-> 5 : 70 
3 <-> 6 : 90 
3 <-> 7 : 150 
4 <-> 5 : 274 
4 <-> 6 : 294 
4 <-> 7 : 82 
5 <-> 6 : 52 
5 <-> 7 : 216 
6 <-> 7 : 236 

0 -> ? -> ? -> ? -> ? -> ?
such that eventually all values appear in ? at some point or other
0 -> 1 -> 0 -> 2 -> 0 -> 3 -> 0 -> 4 -> 0 -> 5 -> 0 -> 6 -> 0 -> 7
as long as sequence has 0,1,2,3,4,5,6,7

sequence must start at 0
no two consecutive values can be the same
0 -> 2 -> 2 -> 2 -> .... is an infinite list with no progression at all

|#

;; ???????
;; (define all-cities?
;;   (let ((zero-to-seven (iota 8)))
;;     (lambda (xs)
;;       (null? (filter (lambda (x) (if x #f #t))
;; 		     (map (lambda (n) (member n zero-to-seven)) xs))))))

;; bit inefficient - iterating over xs 
(define all-cities?
  (lambda (xs)
    (let ((vec (make-vector 8 0))
	  (tot 0))
      (letrec ((foo (lambda (xs)
		      (cond
		       ((null? xs) #f)
		       (#t (let ((val (car xs)))
			     ;; new value seen - record it to total !
			     (when (= 0 (vector-ref vec val))
			       (set! tot (+ tot 1)))
			     ;; mark value as seen
			     (vector-set! vec val 1)
			     (foo (cdr xs))))))))
	(foo xs)
	(= tot 8)))))
	       



;; limit 
(define iter
  (let ((zero-to-seven (iota 8))
	(best-cost #f)
	(best-path #f))
    (lambda (max-len)
      (format #t "max-length path ~a ~%" max-len)
      (letrec ((foo (lambda (at len path cost)		      
		      (cond
		       ;; if we return back to base and all cities visited 
		       ((and (= at 0) (all-cities? path))
			(cond
			 ((or (not best-cost) (< cost best-cost))
			  (set! best-cost cost)
			  (set! best-path path)
			  (format #t "best path so far ~a ~%" best-path)
			  (format #t "best cost so far ~a ~%" best-cost)
			  )))
		       ;; cut ! stop looking if current path is already too expensive
		       ((and best-cost (> cost best-cost)) #f)
		       ;; cut ! path too long may be stuck in cyclic loop 0->1->0->1-> etc...
		       ((> len max-len)
			;;(format #t "rejected path = ~a ~%" path)
			#f)
		       (#t
			(map (lambda (x)			       
			       (cond
				;; no two consecutive cities same
				((= x at) #f)				
				(#t 
				 (foo x (+ len 1) (cons x path) (+ cost (hashed-distance-ab at x))))))
			     zero-to-seven))))))
	(let ((count 1)
	      (path '(0))
	      (cost 0))	      
	  (foo 0 count path cost)	  
	  )))))

(define brute
  (lambda ()
    (letrec ((foo (lambda (i)
		    (iter i)
		    (foo (+ i 1)))))
      (foo 1))))



#|

> (brute)
max-length path 1 
max-length path 2 
max-length path 3 
max-length path 4 
max-length path 5 
max-length path 6 
max-length path 7 
max-length path 8 
best path so far (0 7 6 5 4 3 2 1 0) 
best cost so far 1208 
best path so far (0 6 5 7 4 3 2 1 0) 
best cost so far 1200 
best path so far (0 7 4 6 5 3 2 1 0) 
best cost so far 936 
best path so far (0 4 7 6 5 3 2 1 0) 
best cost so far 884 
best path so far (0 7 4 3 6 5 2 1 0) 
best cost so far 832 
best path so far (0 4 7 3 6 5 2 1 0) 
best cost so far 780 
best path so far (0 4 7 3 5 6 2 1 0) 
best cost so far 748 
best path so far (0 7 3 5 6 2 4 1 0) 
best cost so far 728 
best path so far (0 3 5 6 2 7 4 1 0) 
best cost so far 720  ................... 720 ....... ACCEPTED ........
max-length path 9 
max-length path 10 
max-length path 11 
max-length path 12 
max-length path 13 

|#
