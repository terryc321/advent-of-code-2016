
#|

since big vector only stores at what step we are
we traverse entire vector at each step
expectation solution is fairly low maybe less than 1000
u16 vector should be more than enough

or possibly a u8 vector


|#


(import (chicken format)) ;; format #t 
(import (chicken syntax)) ;; ?
(import (chicken pretty-print)) ;; pp pretty-print
(import procedural-macros) ;; macros 
(import expand-full) ;; pp + expand* macro expander
;; (import srfi-69) ;; hash tables
(import srfi-4)


;;--------------------------------------------------------

#|


The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.
1 (sg sm pg pm)

The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
2 (tg rg rm cg cm)

The third floor contains a thulium-compatible microchip.
3 (tm)

The fourth floor contains nothing relevant.
4 ()

4 ()
3 (tm)
2 (tg rg rm cg cm)
1 (sg sm pg pm *** eg em dg dm ***)  extra 4 parts

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


;; --------------------------------------------------------------------------------

;; syms are everything except e elevator
;; e sg sm pg pm tg tm rg rm cg cm eg em dg dm)
;;
;; fn calls use extra n track how many steps took to reach
;; (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)
 (define (gen)
   (letrec ((syms (reverse '(sg sm pg pm tg tm rg rm cg cm eg em dg dm)))
	    (res '())
	    (add-clause (lambda (x) (set! res (cons x res))))
	    (up-1 (lambda (sym)
		    (add-clause
		     `(when (and (< e 4) (= e ,sym))
			(let ((,sym (+ ,sym 1))
			      (e (+ e 1)))
			  (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n))))))
	    (down-1 (lambda (sym)
		      (add-clause
		       `(when (and (> e 1) (= e ,sym))
			  (let ((,sym (- ,sym 1))
				(e (- e 1)))
			  (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n))))))
	    (up-2 (lambda (sym sym2)
		    (add-clause
		     `(when (and (< e 4) (= e ,sym)(= e ,sym2))
			(let ((,sym (+ ,sym 1))
			      (,sym2 (+ ,sym2 1))
			      (e (+ e 1)))
			  (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n))))))
	    (down-2 (lambda (sym sym2)
		      (add-clause
		       `(when (and (> e 1) (= e ,sym)(= e ,sym2))
			  (let ((,sym (- ,sym 1))
				(,sym2 (- ,sym2 1))
				(e (- e 1)))
			  (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n))))))
	    )	     
     ;; all one piece moves
     (dolist (s syms)  (up-1 s))
     (dolist (s syms)  (down-1 s))
     (let ((seen '()))
       (dolist (s syms)
	       (dolist (t syms)
		       (cond
			((eq? s t) #f)
			((or (member (list s t) seen)
			     (member (list t s) seen))
			 #f)
			(#t
			 (up-2 s t)
			 (down-2 s t)
			 (set! seen (cons (list s t) seen)))))))
     ;; all two piece moves
     ;; (foo3 syms 1)
     (set! res (cons 'begin res))
     res
     ))


;; (define-syntax gen-macro
;;   (er-macro-transformer
;;    (lambda (form rename compare?)
;;      (gen))))

;; (define-macro (gen-macro)
;;   (gen))

;;---------------------------------------------------------------------------



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

;; e sg sm pg pm tg tm rg rm cg cm eg em dg dm)
(define (encode e sg sm pg pm tg tm rg rm cg cm eg em dg dm)
  (+ (* (expt 4 0) (- dm 1))
     (* (expt 4 1) (- dg 1))
     (* (expt 4 2) (- em 1))
     (* (expt 4 3) (- eg 1))
     (* (expt 4 4) (- cm 1))
     (* (expt 4 5) (- cg 1))
     (* (expt 4 6) (- rm 1))
     (* (expt 4 7) (- rg 1))
     (* (expt 4 8) (- tm 1))
     (* (expt 4 9) (- tg 1))
     (* (expt 4 10) (- pm 1))
     (* (expt 4 11) (- pg 1))
     (* (expt 4 12) (- sm 1))
     (* (expt 4 13) (- sg 1))
     (* (expt 4 14) (- e 1))
     ))

;; we can multiple definitions in sequence with let* 
;; (let* ((x 1)(y x)(x (- x 1))(z x)) (list x y z))
;; => (0 1 0)

;; ------ ok so i am very surprised this worked first time decoding , the encoded value
;; e sg sm pg pm tg tm rg rm cg cm eg em dg dm
(define (decode n)
  (let ((x n))
    (let* ((dm (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (dg (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (em (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (eg (+ 1 (remainder x 4)))
	   (x (floor (/ x 4)))
	   (cm (+ 1 (remainder x 4)))
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
      (vector e sg sm pg pm tg tm rg rm cg cm eg em dg dm))))



;; e sg sm pg pm tg tm rg rm cg cm eg em dg dm
(define check-levels
  (let ((count 0))
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
			(dolist (eg levels)
			(dolist (em levels)
			(dolist (dg levels)
			(dolist (dm levels)

				
				;;           e hm hg lm lg 
				;; (format #t "~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a => ~a~%"
				;; 	     e sg sm pg pm tg tm rg rm cg cm count )
				
				;;(if #t (error "pig check levels"))
				
				(when (zero? (modulo count 1000000))
				  (format #t "encoding / decoding checks ~a ~%" count))
				;; e sg sm pg pm tg tm rg rm cg cm eg em dg dm				
				(let ((coded  (encode e sg sm pg pm tg tm rg rm cg cm eg em dg dm)))
				  (assert (= coded count))
				  (if (not (= coded count)) (error "check-levels encode error"))
				  (let ((decoded (decode coded)))
				    (assert (equal? (vector->list decoded)
						    (list e sg sm pg pm tg tm rg rm cg cm eg em dg dm)))
				    (if (not (equal? (vector->list decoded)
						     (list e sg sm pg pm tg tm rg rm cg cm eg em dg dm)))
					(error "check-levels decode error"))

				    #t))
				;;(hash-table-set! hash (list e sg sm pg pm tg tm rg rm cg cm) count)
				;;(hash-table-set! hash count (vector e sg sm pg pm tg tm rg rm cg cm))
				(incf count))))))))))))))))))
	     (lookup (lambda (x) #f)))
		       ;; (hash-table-ref hash x))))
      (create)
      lookup))))


;; (format #t "checking decodnign... ~%")
;; (check-levels)

(define hax (lambda (x)
	      (cond
	       ((integer? x) (decode x))
	       ((pair? x) (apply encode x))
	       (#t (error "hax wrong type")))))



;; rather than use a hash table 
;;   
;;(define hax (make-levels))
;; (list    e sg sm pg pm tg tm rg rm cg cm)
;;(length '(1  1 1   3  2  1  2  1  4  1  2))   => 11 items in length 
;; (expt 4 11) 4194304



;;
;; e sg sm pg pm tg tm rg rm cg cm eg em dg dm
(define (invalid e sg sm pg pm tg tm rg rm cg cm eg em dg dm)
  (cond
   ((and (not (= sm sg)) (or (= sm    pg tg rg cg eg dg))) #t)
   ((and (not (= pm pg)) (or (= pm sg    tg rg cg eg dg))) #t)
   ((and (not (= tm tg)) (or (= tm sg pg    rg cg eg dg))) #t)
   ((and (not (= rm rg)) (or (= rm sg pg tg    cg eg dg))) #t)
   ((and (not (= cm cg)) (or (= cm sg pg tg rg    eg dg))) #t)
   ((and (not (= em eg)) (or (= em sg pg tg rg cg    dg))) #t)
   ((and (not (= dm dg)) (or (= dm sg pg tg rg cg eg   ))) #t)   
   (#t #f)))


;; ------------------------------------------------------
;; ------------- TODO : up / down ----------------------
;; begin-for-syntax allows (gen) to be defined at macro expansion time ??
(define (next e sg sm pg pm tg tm rg rm cg cm eg em dg dm n fn)
  ;; paste gen code here when done ...
(begin
  (when (and (> e 1) (= e sm) (= e sg))
        (let ((sm (- sm 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e sm) (= e sg))
        (let ((sm (+ sm 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e pg) (= e sg))
        (let ((pg (- pg 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e pg) (= e sg))
        (let ((pg (+ pg 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e pg) (= e sm))
        (let ((pg (- pg 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e pg) (= e sm))
        (let ((pg (+ pg 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e pm) (= e sg))
        (let ((pm (- pm 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e pm) (= e sg))
        (let ((pm (+ pm 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e pm) (= e sm))
        (let ((pm (- pm 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e pm) (= e sm))
        (let ((pm (+ pm 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e pm) (= e pg))
        (let ((pm (- pm 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e pm) (= e pg))
        (let ((pm (+ pm 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e tg) (= e sg))
        (let ((tg (- tg 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e tg) (= e sg))
        (let ((tg (+ tg 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e tg) (= e sm))
        (let ((tg (- tg 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e tg) (= e sm))
        (let ((tg (+ tg 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e tg) (= e pg))
        (let ((tg (- tg 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e tg) (= e pg))
        (let ((tg (+ tg 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e tg) (= e pm))
        (let ((tg (- tg 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e tg) (= e pm))
        (let ((tg (+ tg 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e tm) (= e sg))
        (let ((tm (- tm 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e tm) (= e sg))
        (let ((tm (+ tm 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e tm) (= e sm))
        (let ((tm (- tm 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e tm) (= e sm))
        (let ((tm (+ tm 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e tm) (= e pg))
        (let ((tm (- tm 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e tm) (= e pg))
        (let ((tm (+ tm 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e tm) (= e pm))
        (let ((tm (- tm 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e tm) (= e pm))
        (let ((tm (+ tm 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e tm) (= e tg))
        (let ((tm (- tm 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e tm) (= e tg))
        (let ((tm (+ tm 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rg) (= e sg))
        (let ((rg (- rg 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rg) (= e sg))
        (let ((rg (+ rg 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rg) (= e sm))
        (let ((rg (- rg 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rg) (= e sm))
        (let ((rg (+ rg 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rg) (= e pg))
        (let ((rg (- rg 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rg) (= e pg))
        (let ((rg (+ rg 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rg) (= e pm))
        (let ((rg (- rg 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rg) (= e pm))
        (let ((rg (+ rg 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rg) (= e tg))
        (let ((rg (- rg 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rg) (= e tg))
        (let ((rg (+ rg 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rg) (= e tm))
        (let ((rg (- rg 1)) (tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rg) (= e tm))
        (let ((rg (+ rg 1)) (tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rm) (= e sg))
        (let ((rm (- rm 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rm) (= e sg))
        (let ((rm (+ rm 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rm) (= e sm))
        (let ((rm (- rm 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rm) (= e sm))
        (let ((rm (+ rm 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rm) (= e pg))
        (let ((rm (- rm 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rm) (= e pg))
        (let ((rm (+ rm 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rm) (= e pm))
        (let ((rm (- rm 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rm) (= e pm))
        (let ((rm (+ rm 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rm) (= e tg))
        (let ((rm (- rm 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rm) (= e tg))
        (let ((rm (+ rm 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rm) (= e tm))
        (let ((rm (- rm 1)) (tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rm) (= e tm))
        (let ((rm (+ rm 1)) (tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rm) (= e rg))
        (let ((rm (- rm 1)) (rg (- rg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rm) (= e rg))
        (let ((rm (+ rm 1)) (rg (+ rg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cg) (= e sg))
        (let ((cg (- cg 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cg) (= e sg))
        (let ((cg (+ cg 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cg) (= e sm))
        (let ((cg (- cg 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cg) (= e sm))
        (let ((cg (+ cg 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cg) (= e pg))
        (let ((cg (- cg 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cg) (= e pg))
        (let ((cg (+ cg 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cg) (= e pm))
        (let ((cg (- cg 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cg) (= e pm))
        (let ((cg (+ cg 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cg) (= e tg))
        (let ((cg (- cg 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cg) (= e tg))
        (let ((cg (+ cg 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cg) (= e tm))
        (let ((cg (- cg 1)) (tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cg) (= e tm))
        (let ((cg (+ cg 1)) (tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cg) (= e rg))
        (let ((cg (- cg 1)) (rg (- rg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cg) (= e rg))
        (let ((cg (+ cg 1)) (rg (+ rg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cg) (= e rm))
        (let ((cg (- cg 1)) (rm (- rm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cg) (= e rm))
        (let ((cg (+ cg 1)) (rm (+ rm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cm) (= e sg))
        (let ((cm (- cm 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cm) (= e sg))
        (let ((cm (+ cm 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cm) (= e sm))
        (let ((cm (- cm 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cm) (= e sm))
        (let ((cm (+ cm 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cm) (= e pg))
        (let ((cm (- cm 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cm) (= e pg))
        (let ((cm (+ cm 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cm) (= e pm))
        (let ((cm (- cm 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cm) (= e pm))
        (let ((cm (+ cm 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cm) (= e tg))
        (let ((cm (- cm 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cm) (= e tg))
        (let ((cm (+ cm 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cm) (= e tm))
        (let ((cm (- cm 1)) (tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cm) (= e tm))
        (let ((cm (+ cm 1)) (tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cm) (= e rg))
        (let ((cm (- cm 1)) (rg (- rg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cm) (= e rg))
        (let ((cm (+ cm 1)) (rg (+ rg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cm) (= e rm))
        (let ((cm (- cm 1)) (rm (- rm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cm) (= e rm))
        (let ((cm (+ cm 1)) (rm (+ rm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cm) (= e cg))
        (let ((cm (- cm 1)) (cg (- cg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cm) (= e cg))
        (let ((cm (+ cm 1)) (cg (+ cg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e eg) (= e sg))
        (let ((eg (- eg 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e eg) (= e sg))
        (let ((eg (+ eg 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e eg) (= e sm))
        (let ((eg (- eg 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e eg) (= e sm))
        (let ((eg (+ eg 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e eg) (= e pg))
        (let ((eg (- eg 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e eg) (= e pg))
        (let ((eg (+ eg 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e eg) (= e pm))
        (let ((eg (- eg 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e eg) (= e pm))
        (let ((eg (+ eg 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e eg) (= e tg))
        (let ((eg (- eg 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e eg) (= e tg))
        (let ((eg (+ eg 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e eg) (= e tm))
        (let ((eg (- eg 1)) (tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e eg) (= e tm))
        (let ((eg (+ eg 1)) (tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e eg) (= e rg))
        (let ((eg (- eg 1)) (rg (- rg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e eg) (= e rg))
        (let ((eg (+ eg 1)) (rg (+ rg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e eg) (= e rm))
        (let ((eg (- eg 1)) (rm (- rm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e eg) (= e rm))
        (let ((eg (+ eg 1)) (rm (+ rm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e eg) (= e cg))
        (let ((eg (- eg 1)) (cg (- cg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e eg) (= e cg))
        (let ((eg (+ eg 1)) (cg (+ cg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e eg) (= e cm))
        (let ((eg (- eg 1)) (cm (- cm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e eg) (= e cm))
        (let ((eg (+ eg 1)) (cm (+ cm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e em) (= e sg))
        (let ((em (- em 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e em) (= e sg))
        (let ((em (+ em 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e em) (= e sm))
        (let ((em (- em 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e em) (= e sm))
        (let ((em (+ em 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e em) (= e pg))
        (let ((em (- em 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e em) (= e pg))
        (let ((em (+ em 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e em) (= e pm))
        (let ((em (- em 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e em) (= e pm))
        (let ((em (+ em 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e em) (= e tg))
        (let ((em (- em 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e em) (= e tg))
        (let ((em (+ em 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e em) (= e tm))
        (let ((em (- em 1)) (tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e em) (= e tm))
        (let ((em (+ em 1)) (tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e em) (= e rg))
        (let ((em (- em 1)) (rg (- rg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e em) (= e rg))
        (let ((em (+ em 1)) (rg (+ rg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e em) (= e rm))
        (let ((em (- em 1)) (rm (- rm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e em) (= e rm))
        (let ((em (+ em 1)) (rm (+ rm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e em) (= e cg))
        (let ((em (- em 1)) (cg (- cg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e em) (= e cg))
        (let ((em (+ em 1)) (cg (+ cg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e em) (= e cm))
        (let ((em (- em 1)) (cm (- cm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e em) (= e cm))
        (let ((em (+ em 1)) (cm (+ cm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e em) (= e eg))
        (let ((em (- em 1)) (eg (- eg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e em) (= e eg))
        (let ((em (+ em 1)) (eg (+ eg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg) (= e sg))
        (let ((dg (- dg 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg) (= e sg))
        (let ((dg (+ dg 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg) (= e sm))
        (let ((dg (- dg 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg) (= e sm))
        (let ((dg (+ dg 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg) (= e pg))
        (let ((dg (- dg 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg) (= e pg))
        (let ((dg (+ dg 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg) (= e pm))
        (let ((dg (- dg 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg) (= e pm))
        (let ((dg (+ dg 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg) (= e tg))
        (let ((dg (- dg 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg) (= e tg))
        (let ((dg (+ dg 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg) (= e tm))
        (let ((dg (- dg 1)) (tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg) (= e tm))
        (let ((dg (+ dg 1)) (tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg) (= e rg))
        (let ((dg (- dg 1)) (rg (- rg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg) (= e rg))
        (let ((dg (+ dg 1)) (rg (+ rg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg) (= e rm))
        (let ((dg (- dg 1)) (rm (- rm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg) (= e rm))
        (let ((dg (+ dg 1)) (rm (+ rm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg) (= e cg))
        (let ((dg (- dg 1)) (cg (- cg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg) (= e cg))
        (let ((dg (+ dg 1)) (cg (+ cg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg) (= e cm))
        (let ((dg (- dg 1)) (cm (- cm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg) (= e cm))
        (let ((dg (+ dg 1)) (cm (+ cm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg) (= e eg))
        (let ((dg (- dg 1)) (eg (- eg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg) (= e eg))
        (let ((dg (+ dg 1)) (eg (+ eg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg) (= e em))
        (let ((dg (- dg 1)) (em (- em 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg) (= e em))
        (let ((dg (+ dg 1)) (em (+ em 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e sg))
        (let ((dm (- dm 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e sg))
        (let ((dm (+ dm 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e sm))
        (let ((dm (- dm 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e sm))
        (let ((dm (+ dm 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e pg))
        (let ((dm (- dm 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e pg))
        (let ((dm (+ dm 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e pm))
        (let ((dm (- dm 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e pm))
        (let ((dm (+ dm 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e tg))
        (let ((dm (- dm 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e tg))
        (let ((dm (+ dm 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e tm))
        (let ((dm (- dm 1)) (tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e tm))
        (let ((dm (+ dm 1)) (tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e rg))
        (let ((dm (- dm 1)) (rg (- rg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e rg))
        (let ((dm (+ dm 1)) (rg (+ rg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e rm))
        (let ((dm (- dm 1)) (rm (- rm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e rm))
        (let ((dm (+ dm 1)) (rm (+ rm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e cg))
        (let ((dm (- dm 1)) (cg (- cg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e cg))
        (let ((dm (+ dm 1)) (cg (+ cg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e cm))
        (let ((dm (- dm 1)) (cm (- cm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e cm))
        (let ((dm (+ dm 1)) (cm (+ cm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e eg))
        (let ((dm (- dm 1)) (eg (- eg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e eg))
        (let ((dm (+ dm 1)) (eg (+ eg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e em))
        (let ((dm (- dm 1)) (em (- em 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e em))
        (let ((dm (+ dm 1)) (em (+ em 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm) (= e dg))
        (let ((dm (- dm 1)) (dg (- dg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm) (= e dg))
        (let ((dm (+ dm 1)) (dg (+ dg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e sg))
        (let ((sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e sm))
        (let ((sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e pg))
        (let ((pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e pm))
        (let ((pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e tg))
        (let ((tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e tm))
        (let ((tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rg))
        (let ((rg (- rg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e rm))
        (let ((rm (- rm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cg))
        (let ((cg (- cg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e cm))
        (let ((cm (- cm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e eg))
        (let ((eg (- eg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e em))
        (let ((em (- em 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dg))
        (let ((dg (- dg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (> e 1) (= e dm))
        (let ((dm (- dm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e sg))
        (let ((sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e sm))
        (let ((sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e pg))
        (let ((pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e pm))
        (let ((pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e tg))
        (let ((tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e tm))
        (let ((tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rg))
        (let ((rg (+ rg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e rm))
        (let ((rm (+ rm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cg))
        (let ((cg (+ cg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e cm))
        (let ((cm (+ cm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e eg))
        (let ((eg (+ eg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e em))
        (let ((em (+ em 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dg))
        (let ((dg (+ dg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)))
  (when (and (< e 4) (= e dm))
        (let ((dm (+ dm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm eg em dg dm n))))
)



;; ----------------- ok ----------------
;; make VEC a big unsigned vector , then 0 being all unset
;;


;; 11 entries in the puzzle input
(define vec-limit (- (expt 4 15) 1))
(define vec (make-u8vector (+ vec-limit 1) 0))



;; we have not 
;; a lot of these states are not reachable because ?
;; e sg sm pg pm tg tm rg rm cg cm eg em dg dm)
(define (iterative e sg sm pg pm tg tm rg rm cg cm eg em dg dm)
  (for i 0 vec-limit ;; each entry
       (u8vector-set! vec i 0))
  (forever e sg sm pg pm tg tm rg rm cg cm eg em dg dm))

;; hash-table
;; hash-table-ref/default if know key may not be in hash table
;;
;;
;;

;; e sg sm pg pm tg tm rg rm cg cm eg em dg dm)
(define (forever e sg sm pg pm tg tm rg rm cg cm eg em dg dm)
  (let ((code (hax (list e sg sm pg pm tg tm rg rm cg cm eg em dg dm)))
	(processed 0))
    (format #t "init = ~A ~%" code)
    (u8vector-set! vec code 1)
    (let ((n 0)) ;; n 0 incremented next iteration to 1 , look for any vecs set to 1

      ;; all u8vector-refs are set to zero , so anything above zero means we hit it
    (while (< (u8vector-ref vec vec-limit) 1)
      ;; next iteration
      (set! n (+ n 1))
      (set! processed 0)
      ;; swap 
      (format #t "startting iteration ~a ~%" n)
      (for i 0 vec-limit ;; each entry
	   (let ((k (u8vector-ref vec i)))
	     (when (and (positive? k) (= k n))
	       (incf processed)
	       (let ((c (hax i)))
		 (let ((e  (vector-ref c 0))
		       (sg (vector-ref c 1))
		       (sm (vector-ref c 2))
		       (pg (vector-ref c 3))
		       (pm (vector-ref c 4))
		       (tg (vector-ref c 5))
		       (tm (vector-ref c 6))
		       (rg (vector-ref c 7))
		       (rm (vector-ref c 8))
		       (cg (vector-ref c 9))
		       (cm (vector-ref c 10))
		       (eg (vector-ref c 11))
		       (em (vector-ref c 12))
		       (dg (vector-ref c 13))
		       (dm (vector-ref c 14))		       
		        ;; fn marks state [e ...] with value [n]
		       (fn (lambda (e sg sm pg pm tg tm rg rm cg cm eg em dg dm n)
			     (cond
			      ((invalid e sg sm pg pm tg tm rg rm cg cm eg em dg dm) #f)
			      (#t 
			       (let ((i (hax (list e sg sm pg pm tg tm rg rm cg cm eg em dg dm))))
				 (let ((val (u8vector-ref vec i)))
				   (cond
				    ((zero? val)
				     (u8vector-set! vec i n))))))))))
		   (cond
		    ((invalid e sg sm pg pm tg tm rg rm cg cm eg em dg dm) #f)
		    (#t 
		     ;; is it a valid state ? hopefully so
		     (next e sg sm pg pm tg tm rg rm cg cm eg em dg dm (+ n 1) fn))))))))
      (format #t "processed ~a ~%" processed)
      )
    (let* ((would (u8vector-ref vec vec-limit))
	   (more-likely (- would 1)))
      (format #t "~%found solution [ ~a -> * @ ~a * ] ~%~%" would more-likely)))))



  



;; puzzle initial state 
;; 4 ()
;; 3 (tm)
;; 2 (tg rg rm cg cm)
;; 1 (sg sm pg pm)
;; 1 (sg sm pg pm *** eg em dg dm ***)  extra 4 parts
;;                                   *** Four NEW parts ***
;;   e sg sm pg pm tg tm rg rm cg cm eg em dg dm
;;   1  1  1  1  1  2  3  2  2  2  2 1  1   1  1
;; '(1  1  1  1  1  2  3  2  2  2  2 1  1   1  1)

;; what is initial state of puzzle ?
(define run (lambda ()
	      (letrec
		  ((e 1) ;; elevator on floor 1 
		   (sg 1)
		   (sm 1)
		   (pg 1)
		   (pm 1)
		   (tg 2)
		   (tm 3)
		   (rg 2)
		   (rm 2)
		   (cg 2)
		   (cm 2)
		   (eg 1)
		   (em 1)
		   (dg 1)
		   (dm 1)
		   )
		(iterative e sg sm pg pm tg tm rg rm cg cm eg em dg dm)
		)))


(run)


#|
terry@debian:~/code/advent-of-code/advent-of-code-2016/day11/chicken$ time ./u8-vec-limit
init = 414976 
startting iteration 1 
processed 1 
startting iteration 2 
processed 36 
startting iteration 3 
processed 1505 
startting iteration 4 
processed 8603 
startting iteration 5 
processed 88726 
startting iteration 6 
processed 469882 
startting iteration 7 
processed 943302 
startting iteration 8 
processed 4265410 
startting iteration 9 
processed 3931385 
startting iteration 10 
processed 13894934 
startting iteration 11 
processed 9213786 
startting iteration 12 
processed 20800978 
startting iteration 13 
processed 15489157 
startting iteration 14 
processed 28693026 
startting iteration 15 
processed 22186083 
startting iteration 16 
processed 36853055 
startting iteration 17 
processed 29659869 
startting iteration 18 
processed 43940502 
startting iteration 19 
processed 37471809 
startting iteration 20 
processed 49031433 
startting iteration 21 
processed 43495585 
startting iteration 22 
processed 51473212 
startting iteration 23 
processed 47909819 
startting iteration 24 
processed 51052468 
startting iteration 25 
processed 49536785 
startting iteration 26 
processed 48008476 
startting iteration 27 
processed 48938258 
startting iteration 28 
processed 42905309 
startting iteration 29 
processed 45981034 
startting iteration 30 
processed 36488482 
startting iteration 31 
processed 41298409 
startting iteration 32 
processed 29550868 
startting iteration 33 
processed 35388870 
startting iteration 34 
processed 22787861 
startting iteration 35 
processed 28863171 
startting iteration 36 
processed 16715274 
startting iteration 37 
processed 22356638 
startting iteration 38 
processed 11639518 
startting iteration 39 
processed 16371472 
startting iteration 40 
processed 7665424 
startting iteration 41 
processed 11284122 
startting iteration 42 
processed 4759199 
startting iteration 43 
processed 7283123 
startting iteration 44 
processed 2771593 
startting iteration 45 
processed 4371205 
startting iteration 46 
processed 1503502 
startting iteration 47 
processed 2418906 
startting iteration 48 
processed 755106 
startting iteration 49 
processed 1224181 
startting iteration 50 
processed 346787 
startting iteration 51 
processed 558434 
startting iteration 52 
processed 143920 
startting iteration 53 
processed 225428 
startting iteration 54 
processed 53368 
startting iteration 55 
processed 79464 
startting iteration 56 
processed 16828 
startting iteration 57 
processed 23352 
startting iteration 58 
processed 4326 
startting iteration 59 
processed 5334 
startting iteration 60 
processed 819 
startting iteration 61 
processed 875 

found solution [ 62 -> * @ 61 * ] 


real	202m38.922s  <<<< circa 3hours 20 mins approx
user	201m58.017s
sys	0m40.636s
terry@debian:~/code/advent-of-code/advent-of-code-2016/day11/chicken$ 



|#
