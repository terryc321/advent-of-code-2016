

(import (chicken format)) ;; format #t 
(import (chicken syntax)) ;; ?
(import (chicken pretty-print)) ;; pp pretty-print
(import procedural-macros) ;; macros 
(import expand-full) ;; pp + expand* macro expander
(import srfi-69) ;; hash tables


;;--------------------------------------------------------

#|

since each generator or microchip or elevator must be on one of the floors 1 2 3 or 4
means 4 ^ p states
where p = 1 + count(generators) + count(microchips)

in example given 2 generators , 2 microchips , elevator
      p = 1 + 2 + 2
      p = 5

(expt 4 5 )  1024

array of 1024 in size
 0 to 1023 if wish
 fill array with False #f meaning never visited this before 
 when we hit a state , encode it to a single integer , look into array 
 if see False , put our current STEP count N  into it
 if see value already and is less than STEP count , know already reached this point , we can ignore
 breadth first search , 
    mark first start point as 1 say 
    every reachable state mark as 2
    scan array for every reachable state 2 and mark each new reachable state as 3
    so on and so forth 


The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.

As a diagram (F# for a Floor number, E for Elevator, H for Hydrogen, L for Lithium, M for Microchip, and G for Generator), the initial state looks like this:

F4 .  .  .  .  .  
F3 .  .  .  LG .  
F2 .  HG .  .  .  
F1 E  .  HM .  LM 

representation of state 
'((e hm lm)(hg)(lg)())

set of states done

set of states pending

start state = initial state
say this is start state
'((e hm lm)(hg)(lg)())

     e hm hg lm lg 
[ID (1 1   2  1  3)  ID-PREV  ]  
elevator on 1 , hm on 1 , hg on 2 , lm on 1 , lg on 3
ID may represent state-id : identifies the state itself
this way we can lay bread crumbs to find our way back 

bootstrap process
working has (#(1 1 2 1 3))
is the current state already been exhausted ?
find all reachable states from current state
rather for each reachable state from current-state is it      

|#

(define-macro (incf x)
  `(set! ,x (+ ,x 1)))

(let ((a 1))
  (incf a)
  a)

(define-macro (swap! x y)
  (let ((tmp (gensym "tmp")))
    `(let ((,tmp ,x))
       (set! ,x ,y)
       (set! ,y ,tmp))))

(let ((a 1)
      (b 2))      
  (swap! a b)
  (list a b))

;; we can recover an unspecific by just executing a begin 

(define-macro (dolist varlist . body)
  (let ((var (car varlist))
	(xs (car (cdr varlist)))
	(fn (gensym "fn"))
	(ls (gensym "ls"))
	)
    `(letrec ((,fn (lambda (,ls)
		     (cond
		      ((null? ,ls) (begin))
		      (#t (let ((,var (car ,ls)))
			    ,@body
			    (,fn (cdr ,ls))))))))
       (,fn ,xs))))

;; (pp (expand* '(dolist (x '(1 2 3 4 5)) (format #t "x=  ~a ~%" x))))
;;     
;; (dolist (x '(1 2 3 4 5)) (dolist (y '(1 2 3 4 5))
;; 				 (format #t "x=  ~a y= ~a  ~%" x y)))

;;(expand* '(swap x y))

;; for i from 0 to 1023 inclusive
(define-macro (for var low high . body)
  (let ((fn (gensym "fn")))
    `(letrec ((,fn (lambda (,var)
		     (cond
		      ((> ,var ,high) (begin))
		      (#t ,@body
			  (,fn (+ ,var 1)))))))
       (,fn ,low))))

;; (for i 0 10 (format #t " i = ~a ~% " i ))
;; (for i 0 10 (for j 0 10 (format #t " i = ~a : j = ~a ~% " i j)))

;; while loop
(define-macro (while con . body)
  (let ((fn (gensym "fn")))
    `(letrec ((,fn (lambda ()
		     (cond
		      ((not ,con) (begin))
		      (#t ,@body
			  (,fn))))))
       (,fn))))

;; (let ((i 0))
;;   (while (< i 10) (format #t "hello world! ~a~%" i)
;; 	 (set! i (+ i 1))))





;; if i want to resume , from current state
;; pass continuation both next state and some way to run next states again with next possible
;; states
;; what if no next state ?
;;


;; ---- specific to example hm hg lm lg   ---- only ---------

(define levels '(1 2 3 4))

(define make-levels
  (let ((count 0)
	(hash (make-hash-table #:test equal?)))
    (lambda ()
    (letrec ((create  (lambda ()
			(set! count 0)
			(dolist (e levels)
				(dolist (hm levels)
					(dolist (hg levels)
						(dolist (lm levels)
							(dolist (lg levels)
								;;           e hm hg lm lg 
								;; (format #t "~a ~a ~a ~a ~a  => ~a~%"
								;; 	e hm hg lm lg count)
								(hash-table-set! hash (list e hm hg lm lg) count)
								(hash-table-set! hash count (vector e hm hg lm lg))
								(incf count))))))))
	     (lookup (lambda (x)
		       (hash-table-ref hash x))))
      (create)
      lookup))))

     
(define hax (make-levels))


(define (invalid e hm hg lm lg)
  ;;(format #t "ivalid ok ~%")
  (cond
   ((and (not (= hm hg)) (or (= hm lg))) #t)
   ((and (not (= lm lg)) (or (= lm hg))) #t)
   (#t #f)))


(define (down e hm hg lm lg n fn)
  ;;(format #t "down ok ~%")
  (when (> e 1)
    (when (= hm e)
      (let ((e (- e 1)) (hm (- hm 1)))
	(fn e hm hg lm lg n)))
    (when (= hg e)
      (let ((e (- e 1)) (hg (- hg 1)))
	(fn e hm hg lm lg n)))
    (when (= lm e)
      (let ((e (- e 1)) (lm (- lm 1)))
	(fn e hm hg lm lg n)))
    (when (= lg e)
      (let ((e (- e 1)) (lg (- lg 1)))
	(fn e hm hg lm lg n)))
    (when (and (= hm e) (= hg e))
      (let ((e (- e 1)) (hm (- hm 1)) (hg (- hg 1)))
        (fn e hm hg lm lg n)))
    (when (and (= hm e) (= lm e))
      (let ((e (- e 1)) (hm (- hm 1)) (lm (- lm 1)))
        (fn e hm hg lm lg n)))
    (when (and (= hm e) (= lg e))
      (let ((e (- e 1)) (hm (- hm 1)) (lg (- lg 1)))
        (fn e hm hg lm lg n)))
    (when (and (= hg e) (= lm e))
      (let ((e (- e 1)) (hg (- hg 1)) (lm (- lm 1)))
        (fn e hm hg lm lg n)))
    (when (and (= hg e) (= lg e))
      (let ((e (- e 1)) (hg (- hg 1)) (lg (- lg 1)))
        (fn e hm hg lm lg n)))
    (when (and (= lm e) (= lg e))
      (let ((e (- e 1)) (lm (- lm 1)) (lg (- lg 1)))
        (fn e hm hg lm lg n)))))


(define (up e hm hg lm lg n fn)
  ;;(format #t "up ok ~%")
  (when (< e 4)
    (when (= hm e)
      (let ((e (+ e 1)) (hm (+ hm 1))) (fn e hm hg lm lg n)))
    (when (= hg e)
      (let ((e (+ e 1)) (hg (+ hg 1))) (fn e hm hg lm lg n)))
    (when (= lm e)
      (let ((e (+ e 1)) (lm (+ lm 1))) (fn e hm hg lm lg n)))
    (when (= lg e)
      (let ((e (+ e 1)) (lg (+ lg 1))) (fn e hm hg lm lg n)))
    (when (and (= hm e) (= hg e))
      (let ((e (+ e 1)) (hm (+ hm 1)) (hg (+ hg 1)))
        (fn e hm hg lm lg n)))
    (when (and (= hm e) (= lm e))
      (let ((e (+ e 1)) (hm (+ hm 1)) (lm (+ lm 1)))
        (fn e hm hg lm lg n)))
    (when (and (= hm e) (= lg e))
      (let ((e (+ e 1)) (hm (+ hm 1)) (lg (+ lg 1)))
        (fn e hm hg lm lg n)))
    (when (and (= hg e) (= lm e))
      (let ((e (+ e 1)) (hg (+ hg 1)) (lm (+ lm 1)))
        (fn e hm hg lm lg n)))
    (when (and (= hg e) (= lg e))
      (let ((e (+ e 1)) (hg (+ hg 1)) (lg (+ lg 1)))
        (fn e hm hg lm lg n)))
    (when (and (= lm e) (= lg e))
      (let ((e (+ e 1)) (lm (+ lm 1)) (lg (+ lg 1)))
        (fn e hm hg lm lg n)))))


;; (define next-states
;;   (let ((count 0))
;;     (lambda (e hm hg lm lg fn n)
;;       (cond
;;        ((invalid e hm hg lm lg) #f)
;;        (#t
;; 	(format #t " state count [~a] : ~a ~a ~a ~a ~a ~a ~%"
;; 		count  e  hm hg lm lg n)
;; 	(incf count)
;; 	(up e hm hg lm lg fn (+ n 1))
;; 	(down e hm hg lm lg fn (+ n 1)))))))


(define vec (make-vector 1024 #f))

;; we have not 
;; a lot of these states are not reachable because ?
(define (iterative e hm hg lm lg n)
  (set! vec (make-vector 1024))
  (for i 0 1023 ;; each entry
       (vector-set! vec i #f))
  (let ((code (hax '(1 1 2 1 3))))
    (format #t "init = ~A ~%" code)
    (vector-set! vec code 0)
    (forever 1 1 2 1 3)))

(define (forever e hm hg lm lg)
  (let ((n -1))
    (while (not (vector-ref vec 1023))
      ;; next iteration
      (set! n (+ n 1))      
      (for i 0 1023 ;; each entry
	   (let ((k (vector-ref vec i)))
	     (when (and (integer? k) (= k n))
	       (let ((c (hax i)))
		 (let ((e  (vector-ref c 0))
		       (hm (vector-ref c 1))
		       (hg (vector-ref c 2))
		       (lm (vector-ref c 3))
		       (lg (vector-ref c 4))
		       (fn (lambda (e hm hg lm lg n) ;; marks state [e hm hg lm lg] with value [n]
			     (cond
			      ((invalid e hm hg lm lg) #f)
			      (#t 
			       (let ((i (hax (list e hm hg lm lg))))
				 (let ((val (vector-ref vec i)))
				   (cond
				    ((not val)
				     (vector-set! vec i n))))))))))
		   (cond
		    ((invalid e hm hg lm lg) #f)
		    (#t 
		     ;; is it a valid state ? hopefully so
		     (up e hm hg lm lg (+ n 1) fn)
		     (down e hm hg lm lg (+ n 1) fn)))))))))
    (format #t "found solution ~a ~%" (vector-ref vec 1023))))




;; initial level ?
;;      e hm hg lm lg 
;; [ID (1 1   2  1  3)  ID-PREV  ]  
(define run (lambda ()
	      (set! vec (make-vector 1024 #f))
	      (letrec
		  ((e 1)
		   (hm 1)
		   (hg 2)
		   (lm 1)
		   (lg 3)
		   (n 1))
		   ;; (vec (make-vector 1024 #f))		   
		(iterative e hm hg lm lg n)
		(let ((fst (vector-ref vec (hax (list 4 4 4 4 4)))))
		  (format #t "fst = ~a ~%" fst)
		  ))))






#|
huh? @859 : stored 106 : current 104  
huh? @22 : stored 103 : current 93  
huh? @70 : stored 103 : current 93  
huh? @82 : stored 103 : current 93  
fst = 66 

... forget to incorporate rules where chips are fried  !


|#

