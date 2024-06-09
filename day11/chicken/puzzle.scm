

(import (chicken format)) ;; format #t 
(import (chicken syntax)) ;; ?
(import (chicken pretty-print)) ;; pp pretty-print
(import procedural-macros) ;; macros 
(import expand-full) ;; pp + expand* macro expander
(import srfi-69) ;; hash tables


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
1 (sg sm pg pm)



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
 (define (gen)
   (letrec ((syms (reverse '(sg sm pg pm tg tm rg rm cg cm)))
	    (res '())
	    (add-clause (lambda (x) (set! res (cons x res))))
	    (up-1 (lambda (sym)
		    (add-clause
		     `(when (and (< e 4) (= e ,sym))
			(let ((,sym (+ ,sym 1))
			      (e (+ e 1)))
			  (fn e sg sm pg pm tg tm rg rm cg cm n))))))
	    (down-1 (lambda (sym)
		      (add-clause
		       `(when (and (> e 1) (= e ,sym))
			  (let ((,sym (- ,sym 1))
				(e (- e 1)))
			    (fn e sg sm pg pm tg tm rg rm cg cm n))))))
	    (up-2 (lambda (sym sym2)
		    (add-clause
		     `(when (and (< e 4) (= e ,sym)(= e ,sym2))
			(let ((,sym (+ ,sym 1))
			      (,sym2 (+ ,sym2 1))
			      (e (+ e 1)))
			  (fn e sg sm pg pm tg tm rg rm cg cm n))))))
	    (down-2 (lambda (sym sym2)
		      (add-clause
		       `(when (and (> e 1) (= e ,sym)(= e ,sym2))
			  (let ((,sym (- ,sym 1))
				(,sym2 (- ,sym2 1))
				(e (- e 1)))
			    (fn e sg sm pg pm tg tm rg rm cg cm n))))))
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
      (vector e sg sm pg pm tg tm rg rm cg cm))))

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
				    (assert (equal? (vector->list decoded)
						    (list e sg sm pg pm tg tm rg rm cg cm)))
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
;; (list    e sg sm pg pm tg tm rg rm cg cm)
;;(length '(1  1 1   3  2  1  2  1  4  1  2))   => 11 items in length 
;; (expt 4 11) 4194304


;; ok 
(define (invalid e sg sm pg pm tg tm rg rm cg cm)
  ;;(format #t "ivalid ok ~%")
  (cond
   ((and (not (= sm sg)) (or (= sm    pg tg rg cg))) #t)
   ((and (not (= pm pg)) (or (= pm sg    tg rg cg))) #t)
   ((and (not (= tm tg)) (or (= tm sg pg    rg cg))) #t)
   ((and (not (= rm rg)) (or (= rm sg pg tg    cg))) #t)
   ((and (not (= cm cg)) (or (= cm sg pg tg rg   ))) #t)
   (#t #f)))


;; ------------------------------------------------------
;; ------------- TODO : up / down ----------------------
;; begin-for-syntax allows (gen) to be defined at macro expansion time ??
(define (next e sg sm pg pm tg tm rg rm cg cm n fn)
  (begin
  (when (and (> e 1) (= e sm) (= e sg))
        (let ((sm (- sm 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e sm) (= e sg))
        (let ((sm (+ sm 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e pg) (= e sg))
        (let ((pg (- pg 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e pg) (= e sg))
        (let ((pg (+ pg 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e pg) (= e sm))
        (let ((pg (- pg 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e pg) (= e sm))
        (let ((pg (+ pg 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e pm) (= e sg))
        (let ((pm (- pm 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e pm) (= e sg))
        (let ((pm (+ pm 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e pm) (= e sm))
        (let ((pm (- pm 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e pm) (= e sm))
        (let ((pm (+ pm 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e pm) (= e pg))
        (let ((pm (- pm 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e pm) (= e pg))
        (let ((pm (+ pm 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e tg) (= e sg))
        (let ((tg (- tg 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e tg) (= e sg))
        (let ((tg (+ tg 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e tg) (= e sm))
        (let ((tg (- tg 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e tg) (= e sm))
        (let ((tg (+ tg 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e tg) (= e pg))
        (let ((tg (- tg 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e tg) (= e pg))
        (let ((tg (+ tg 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e tg) (= e pm))
        (let ((tg (- tg 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e tg) (= e pm))
        (let ((tg (+ tg 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e tm) (= e sg))
        (let ((tm (- tm 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e tm) (= e sg))
        (let ((tm (+ tm 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e tm) (= e sm))
        (let ((tm (- tm 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e tm) (= e sm))
        (let ((tm (+ tm 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e tm) (= e pg))
        (let ((tm (- tm 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e tm) (= e pg))
        (let ((tm (+ tm 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e tm) (= e pm))
        (let ((tm (- tm 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e tm) (= e pm))
        (let ((tm (+ tm 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e tm) (= e tg))
        (let ((tm (- tm 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e tm) (= e tg))
        (let ((tm (+ tm 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rg) (= e sg))
        (let ((rg (- rg 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rg) (= e sg))
        (let ((rg (+ rg 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rg) (= e sm))
        (let ((rg (- rg 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rg) (= e sm))
        (let ((rg (+ rg 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rg) (= e pg))
        (let ((rg (- rg 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rg) (= e pg))
        (let ((rg (+ rg 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rg) (= e pm))
        (let ((rg (- rg 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rg) (= e pm))
        (let ((rg (+ rg 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rg) (= e tg))
        (let ((rg (- rg 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rg) (= e tg))
        (let ((rg (+ rg 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rg) (= e tm))
        (let ((rg (- rg 1)) (tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rg) (= e tm))
        (let ((rg (+ rg 1)) (tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rm) (= e sg))
        (let ((rm (- rm 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rm) (= e sg))
        (let ((rm (+ rm 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rm) (= e sm))
        (let ((rm (- rm 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rm) (= e sm))
        (let ((rm (+ rm 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rm) (= e pg))
        (let ((rm (- rm 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rm) (= e pg))
        (let ((rm (+ rm 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rm) (= e pm))
        (let ((rm (- rm 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rm) (= e pm))
        (let ((rm (+ rm 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rm) (= e tg))
        (let ((rm (- rm 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rm) (= e tg))
        (let ((rm (+ rm 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rm) (= e tm))
        (let ((rm (- rm 1)) (tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rm) (= e tm))
        (let ((rm (+ rm 1)) (tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rm) (= e rg))
        (let ((rm (- rm 1)) (rg (- rg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rm) (= e rg))
        (let ((rm (+ rm 1)) (rg (+ rg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cg) (= e sg))
        (let ((cg (- cg 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cg) (= e sg))
        (let ((cg (+ cg 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cg) (= e sm))
        (let ((cg (- cg 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cg) (= e sm))
        (let ((cg (+ cg 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cg) (= e pg))
        (let ((cg (- cg 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cg) (= e pg))
        (let ((cg (+ cg 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cg) (= e pm))
        (let ((cg (- cg 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cg) (= e pm))
        (let ((cg (+ cg 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cg) (= e tg))
        (let ((cg (- cg 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cg) (= e tg))
        (let ((cg (+ cg 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cg) (= e tm))
        (let ((cg (- cg 1)) (tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cg) (= e tm))
        (let ((cg (+ cg 1)) (tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cg) (= e rg))
        (let ((cg (- cg 1)) (rg (- rg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cg) (= e rg))
        (let ((cg (+ cg 1)) (rg (+ rg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cg) (= e rm))
        (let ((cg (- cg 1)) (rm (- rm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cg) (= e rm))
        (let ((cg (+ cg 1)) (rm (+ rm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cm) (= e sg))
        (let ((cm (- cm 1)) (sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cm) (= e sg))
        (let ((cm (+ cm 1)) (sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cm) (= e sm))
        (let ((cm (- cm 1)) (sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cm) (= e sm))
        (let ((cm (+ cm 1)) (sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cm) (= e pg))
        (let ((cm (- cm 1)) (pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cm) (= e pg))
        (let ((cm (+ cm 1)) (pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cm) (= e pm))
        (let ((cm (- cm 1)) (pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cm) (= e pm))
        (let ((cm (+ cm 1)) (pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cm) (= e tg))
        (let ((cm (- cm 1)) (tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cm) (= e tg))
        (let ((cm (+ cm 1)) (tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cm) (= e tm))
        (let ((cm (- cm 1)) (tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cm) (= e tm))
        (let ((cm (+ cm 1)) (tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cm) (= e rg))
        (let ((cm (- cm 1)) (rg (- rg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cm) (= e rg))
        (let ((cm (+ cm 1)) (rg (+ rg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cm) (= e rm))
        (let ((cm (- cm 1)) (rm (- rm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cm) (= e rm))
        (let ((cm (+ cm 1)) (rm (+ rm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cm) (= e cg))
        (let ((cm (- cm 1)) (cg (- cg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cm) (= e cg))
        (let ((cm (+ cm 1)) (cg (+ cg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e sg))
        (let ((sg (- sg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e sm))
        (let ((sm (- sm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e pg))
        (let ((pg (- pg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e pm))
        (let ((pm (- pm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e tg))
        (let ((tg (- tg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e tm))
        (let ((tm (- tm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rg))
        (let ((rg (- rg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e rm))
        (let ((rm (- rm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cg))
        (let ((cg (- cg 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (> e 1) (= e cm))
        (let ((cm (- cm 1)) (e (- e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e sg))
        (let ((sg (+ sg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e sm))
        (let ((sm (+ sm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e pg))
        (let ((pg (+ pg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e pm))
        (let ((pm (+ pm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e tg))
        (let ((tg (+ tg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e tm))
        (let ((tm (+ tm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rg))
        (let ((rg (+ rg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e rm))
        (let ((rm (+ rm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cg))
        (let ((cg (+ cg 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))
  (when (and (< e 4) (= e cm))
        (let ((cm (+ cm 1)) (e (+ e 1)))
          (fn e sg sm pg pm tg tm rg rm cg cm n)))))
  ;;(gen-macro)
 

  
;; 11 entries in the puzzle input
(define vec #f)

;; we have not 
;; a lot of these states are not reachable because ?
(define (iterative e sg sm pg pm tg tm rg rm cg cm)
  (set! vec (make-vector (expt 4 11) #f))
  (for i 0 (- (expt 4 11) 1) ;; each entry
       (vector-set! vec i #f))
  (let ((code (hax (list e sg sm pg pm tg tm rg rm cg cm))))
    (format #t "init = ~A ~%" code)
    (vector-set! vec code 0)
    (forever e sg sm pg pm tg tm rg rm cg cm)))

(define (forever e sg sm pg pm tg tm rg rm cg cm)
  (let ((n -1))
    (while (not (vector-ref vec (- (expt 4 11) 1)))
      ;; next iteration
      (set! n (+ n 1))
      (format #t "startting iteration ~a ~%" n)
      (for i 0 (- (expt 4 11) 1) ;; each entry
	   (let ((k (vector-ref vec i)))
	     (when (and (integer? k) (= k n))
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
		        ;; fn marks state [e ...] with value [n]
		       (fn (lambda (e sg sm pg pm tg tm rg rm cg cm n)
			     (cond
			      ((invalid e sg sm pg pm tg tm rg rm cg cm) #f)
			      (#t 
			       (let ((i (hax (list e sg sm pg pm tg tm rg rm cg cm))))
				 (let ((val (vector-ref vec i)))
				   (cond
				    ((not val)
				     (vector-set! vec i n))))))))))
		   (cond
		    ((invalid e sg sm pg pm tg tm rg rm cg cm) #f)
		    (#t 
		     ;; is it a valid state ? hopefully so
		     (next e sg sm pg pm tg tm rg rm cg cm (+ n 1) fn)))))))))
    (format #t "found solution ~a ~%" (vector-ref vec (- (expt 4 11) 1)))))




;; puzzle initial state 
;; 4 ()
;; 3 (tm)
;; 2 (tg rg rm cg cm)
;; 1 (sg sm pg pm)
;;
;;   e sg sm pg pm tg tm rg rm cg cm
;;   1  1  1  1  1  2  3  2  2  2  2
;; '(1  1  1  1  1  2  3  2  2  2  2)

;; what is initial state of puzzle ?
(define run (lambda ()
	      (set! vec #f)
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
		   )
		   ;; (vec (make-vector 1024 #f))		   
		(iterative e sg sm pg pm tg tm rg rm cg cm)
		(let ((fst (vector-ref vec (hax (list 4 4 4 4 4 4 4 4 4 4 4)))))
		  (format #t "fst = ~a ~%" fst)
		  ))))


(run)


#|
init = 1621 
startting iteration 0 
startting iteration 1 
startting iteration 2 
startting iteration 3 
startting iteration 4 
startting iteration 5 
startting iteration 6 
startting iteration 7 
startting iteration 8 
startting iteration 9 
startting iteration 10 
startting iteration 11 
startting iteration 12 
startting iteration 13 
startting iteration 14 
startting iteration 15 
startting iteration 16 
startting iteration 17 
startting iteration 18 
startting iteration 19 
startting iteration 20 
startting iteration 21 
startting iteration 22 
startting iteration 23 
startting iteration 24 
startting iteration 25 
startting iteration 26 
startting iteration 27 
startting iteration 28 
startting iteration 29 
startting iteration 30 
startting iteration 31 
startting iteration 32 
startting iteration 33 
startting iteration 34 
startting iteration 35 
startting iteration 36 
found solution 37 
fst = 37 

real	0m17.430s
user	0m17.402s
sys	0m0.028s
t
|#

