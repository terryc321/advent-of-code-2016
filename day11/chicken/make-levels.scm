

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
;; The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.
;; The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
;; The third floor contains a thulium-compatible microchip.
;; The fourth floor contains nothing relevant.

;; 4 
;; 3 tm
;; 2 tg rg rm cg cm
;; 1 sg sm pg pm

;; e sg sm pg pm tg tm rg rm cg cm

;; ---- specific to example hm hg lm lg   ---- only ---------

(define levels '(1 2 3 4))


(define (encode e sg sm pg pm tg tm rg rm cg cm)
  (+               (- cm 1)
     (* (expt 4 1) (- cg 1))
     (* (expt 4 2) (- rm 1))
     (* (expt 4 3) (- rg 1))
     (* (expt 4 4) (- tm 1))
     (* (expt 4 5) (- tg 1))
     (* (expt 4 6) (- pm 1))
     (* (expt 4 7) (- pg 1))
     (* (expt 4 8) (- sm 1))
     (* (expt 4 9) (- sg 1))
     (* (expt 4 10) (- e 1))
     ))

;; we can multiple definitions in sequence with let* 
;; (let* ((x 1)(y x)(x (- x 1))(z x)) (list x y z))
;; => (0 1 0)

;; ------ ok so i am very surprised this worked first time decoding , the encoded value
(define (decode n)
  (let ((x n))
    (let* ((cm (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (cg (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (rm (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (rg (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (tm (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (tg (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (pm (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (pg (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (sm (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (sg (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (e (+ 1 (remainder x 4))))
      (list e sg sm pg pm tg tm rg rm cg cm))))

(define check-levels
  (let ((count 0)
	(hash (make-hash-table #:test equal?)))
    (lambda ()
    (letrec ((create  (lambda ()
			(set! count 0)
			(dolist (e levels)
			(dolist (sg levels)
			(dolist (sm levels)
			(dolist (pg levels)
			(dolist (pm levels)
			(dolist (tg levels)
			(dolist (tm levels)
			(dolist (rg levels)
			(dolist (rm levels)
			(dolist (cg levels)
			(dolist (cm levels)
				
				;;           e hm hg lm lg 
				;; (format #t "~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a => ~a~%"
				;; 	     e sg sm pg pm tg tm rg rm cg cm count )
				(when (zero? (modulo count 1000))
				  (format #t "encoding / decoding checks ~a ~%" count))
				(let ((coded  (encode e sg sm pg pm tg tm rg rm cg cm)))
				  (assert (= coded count))
				  (let ((decoded (decode coded)))
				    (assert (equal? decoded (list e sg sm pg pm tg tm rg rm cg cm)))
				    #t))
				;;(hash-table-set! hash (list e sg sm pg pm tg tm rg rm cg cm) count)
				;;(hash-table-set! hash count (vector e sg sm pg pm tg tm rg rm cg cm))
				(incf count))))))))))))))
	     (lookup (lambda (x)
		       (hash-table-ref hash x))))
      (create)
      lookup))))


;;(check-levels)

(define hax (lambda (x)
	      (cond
	       ((integer? x) (decode x))
	       ((pair? x) (apply encode x))
	       (#t (error "hax wrong type")))))


;; rather than use a hash table 
;;   
;;(define hax (make-levels))
;;(length '(1 1 1 3 2 1 2 1 4 1 2 )) 
;; (expt 4 11) 4194304



