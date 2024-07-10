
#|
gambit port of chicken code

does gambit have macros ?
gambit has wrong
(format "hello world")
not (format #t "hello world")
|#
(import (srfi 1))
(import (srfi 28))
;;(format #t "hello world!")

;; (import (chicken format))
;; (import (chicken pretty-print))
;; (import matchable)
;; (import procedural-macros)
;; (import expand-full)
;; (import srfi-1)

;;(import-for-syntax (only srfi-1 iota))

  ;; -  0   (cpy a b)  ;; cpy -> jnz 
  ;; -  1   (dec b) ;; dec -> inc ->
  ;; -  2   (cpy a d) ;; cpy -> jnz
  ;; -  3   (cpy 0 a)
  ;; -  4   (cpy b c)
  ;; -  5   (inc a)
  ;; -  6   (dec c)
  ;; -  7   (jnz c -2) ;; jnz -> cpy c -2 :: SKIP 
  ;; -  8   (dec d)
  ;; -  9   (jnz d -5) ;; jnz -> cpy d -5 :: SKIP
  ;; -  10  (dec b)
  ;; -  11  (cpy b c)
  ;; -  12  (cpy c d)
  ;; -  13  (dec d)
  ;; -  14  (inc c)
  ;; -  15  (jnz d -2) ;; jnz -> cpy d -2 :: SkIP -> jnz
  ;; ??  16  (tgl c) ;; tgl -> inc -> dec  ******** only one with 3 states ?? *****
  ;; -  17  (cpy -16 c) ;; cpy -> jnz -> cpy
  ;; -  18  (jnz 1 c)
  ;; -  19  (cpy 98 c)
  ;; -  20  (jnz 86 d)
  ;; -  21  (inc a) ;; inc -> dec -> inc
  ;; -  22  (inc d)
  ;; -  23  (jnz d -2)
  ;; -  24  (inc c)
  ;; -  25  (jnz c -5) ;; jnz -> cpy c -5 :: SKIP -> jnz

;; tog initialized to all zeros at start
(define a 0)
(define b 0)
(define c 0)
(define d 0)
(define i 0)  
(define tog (make-vector 30 0))
(define fvec (make-vector 30 #f))
(define f26 (lambda () 'done))
(define tog0 0)
(define tog1 0)
(define tog2 0)
(define tog3 0)
(define tog4 0)
(define tog5 0)
(define tog6 0)
(define tog7 0)
(define tog8 0)
(define tog9 0)
(define tog10 0)
(define tog11 0)
(define tog12 0)
(define tog13 0)
(define tog14 0)
(define tog15 0)
(define tog16 0)
(define tog17 0)
(define tog18 0)
(define tog19 0)
(define tog20 0)
(define tog21 0)
(define tog22 0)
(define tog23 0)
(define tog24 0)
(define tog25 0)
(define tog26 0) ;; redundant 26th



(define input
  '(
   (cpy a b)  ;; cpy -> jnz 
   (dec b) ;; dec -> inc ->
   (cpy a d) ;; cpy -> jnz
   (cpy 0 a)
   (cpy b c)
   (inc a)
   (dec c)
   (jnz c -2) ;; jnz -> cpy c -2 :: SKIP 
   (dec d)
   (jnz d -5) ;; jnz -> cpy d -5 :: SKIP
   (dec b)
   (cpy b c)
   (cpy c d)
   (dec d)
   (inc c)
   (jnz d -2) ;; jnz -> cpy d -2 :: SkIP -> jnz
   (tgl c) ;; tgl -> inc -> dec  ******** only one with 3 states ?? *****
   (cpy -16 c) ;; cpy -> jnz -> cpy
   (jnz 1 c)
   (cpy 98 c)
   (jnz 86 d)
   (inc a) ;; inc -> dec -> inc
   (inc d)
   (jnz d -2)
   (inc c)
   (jnz c -5) ;; jnz -> cpy c -5 :: SKIP -> jnz
    ))

;; if even then version1() else version2()

;; < setup >
;; i = 0

;; (vector-set! fvec 0 (lambda () (if (even? (vector-ref tog i)) (v0a) (v0b))))
;; (define v0a (lambda () (set! b a) (set! i (+ i 1))))
;; (define v0b (lambda ()  ;;(jnz b a) ;; b,a is not known until runtime 
;; 	      (if (not (zero? b)) (set! i (+ i a)) (set! i (+ i 1)))))
;; 
;; ;; 1 dec b
;; (vector-set! fvec 1 (lambda () (if (even? (vector-ref tog i)) (v1a) (v1b))))
;; (define v1a (lambda () (set! b (- b 1)) (set! i (+ i 1)))) ;; dec b
;; (define v1b (lambda () (set! b (+ b 1)) (set! i (+ i 1)))) ;; inc b
;; 
;; ;; 2 cpy a d  => jnz a d
;; (define f2 (lambda () (if (even? (vector-ref tog i)) (v2a) (v2b))))
;; (define v2a (lambda () (set! d a) (set! i (+ i 1)))) ;; cpy a d
;; (define v2b (lambda ()  ;;(jnz a d) ;; a ,d  is not known until runtime 
;; 	      (if (not (zero? a)) (set! i (+ i d)) (set! i (+ i 1)))))
;; (vector-set! fvec 2 f2)

;; (define fvec (list->vector f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14
;;                             f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26)
#|


one argument version
either inc , dec or tgl

(define f1 (lambda () (if (even? (vector-ref tog i)) (v1a) (v1b))))
(vector-set! fvec 1 f1)
(define v1a (lambda () (set! b (- b 1)) (set! i (+ i 1)))) ;; dec b
(define v1b (lambda () (set! b (+ b 1)) (set! i (+ i 1)))) ;; inc b

|#

;;(import-for-syntax (only format))
;; (import-for-syntax (only (chicken format) format))
;; (import-for-syntax (only bindings bind))

 (define bug
   (lambda (s)
     (format #t "inside [~a] : a[~a] b[~a] c[~a] d[~a] i[~a] tog[~a][~a] ~%" s a b c d i i (vector-ref tog i))))

;; redefine cancel 
(define bug (lambda (s) #t))

;; for now .....
;; assuming inc is (inc b)
;; assuming dec is (dec b) 
(begin-for-syntax
 (define compile-inc
   (lambda (n arg rename compare?)
     (let ((fn (string->symbol (format #f "f~a" n)))
	   (togn (string->symbol (format #f "tog~a" n)))
	   (fnext (string->symbol (format #f "f~a" (+ n 1))))
	   (vna (string->symbol (format #f "v~aa" n)))
	   (vnb (string->symbol (format #f "v~ab" n))))
       `(begin
	  (define ,togn 0)
	  (define ,fn (lambda () (if (even?
				      ;;(vector-ref tog ,n)
				      ,togn
				      )
				     (,vna) (,vnb))))
	  (vector-set! fvec ,n ,fn)
	  ;; inc b
	  (define ,vna (lambda () 
			       (set! ,arg (+ ,arg 1))
			       (begin (set! i (+ i 1))
				      (,fnext)))) 
	  ;; dec arg
	  (define ,vnb (lambda ()  (set! ,arg (- ,arg 1))
			       (begin (set! i (+ i 1))
				      (,fnext))))
	  )))))



(begin-for-syntax
 (define compile-dec
   (lambda (n arg rename compare?)
     (let ((fn (string->symbol (format #f "f~a" n)))
	   (togn (string->symbol (format #f "tog~a" n)))
	   (fnext (string->symbol (format #f "f~a" (+ n 1))))
	   (vna (string->symbol (format #f "v~aa" n)))
	   (vnb (string->symbol (format #f "v~ab" n)))
	   (%set! (rename 'set!))
	   (%define (rename 'define))
	   (%lambda (rename 'lambda))
	   (%+ (rename '+))
	   (%- (rename '-))	       
	   (%vector-set! (rename 'vector-set!))
	   (%even? (rename 'even?))
	   (%vector-ref (rename 'vector-ref))
	   (%if (rename 'if)))
       `(begin
	  (,%define ,fn (,%lambda () (,%if (,%even? ,togn ;;(,%vector-ref tog ,n)
						    ) (,vna) (,vnb))))
	  (,%vector-set! fvec ,n ,fn)
	  (,%define ,vna (,%lambda ()  (,%set! ,arg (,%- ,arg 1)) (begin (,%set! i (,%+ i 1)) (,fnext)))) ;; dec b
	  (,%define ,vnb (,%lambda ()  (,%set! ,arg (,%+ ,arg 1)) (begin (,%set! i (,%+ i 1)) (,fnext)))) ;; inc b	      
	  )))))

(begin-for-syntax
 (define toggle-check
   (lambda (alt)
     (map (lambda (n)
	    (let ((togn (string->symbol (format #f "tog~a" n))))
	      `(when (= ,alt ,n) (set! ,togn (+ 1 ,togn)))))
	  (iota 26)))))


;;(pp (expand* (toggle-check '3)))



#|
;; tgl x
;;  => inc => dec => inc repeat infinitely
|#
(begin-for-syntax
 (define compile-tgl
   (lambda (n arg rename compare?)
     (let ((fn (string->symbol (format #f "f~a" n)))
	   (togn (string->symbol (format #f "tog~a" n)))
	   (fnext (string->symbol (format #f "f~a" (+ n 1))))
	   (vna (string->symbol (format #f "v~aa" n)))
	   (vnb (string->symbol (format #f "v~ab" n)))
	   (%set! (rename 'set!))
	   (%define (rename 'define))
	   (%lambda (rename 'lambda))
	   (%+ (rename '+))
	   (%- (rename '-))	       
	   (%vector-set! (rename 'vector-set!))
	   (%even? (rename 'even?))
	   (%zero? (rename 'zero?))
	   (%vector-ref (rename 'vector-ref))
	   (%cond (rename 'cond))
	   (%and (rename 'and))	   
	   (%if (rename 'if)))
       `(begin
	  (,%define ,fn
		    (,%lambda ()
			      (,%cond
			       ((,%zero? ,togn ;;(,%vector-ref tog ,n)
					 )
				(let ((alt (+ i ,arg)))
				  (,%cond
				   ((,%and (>= alt 0) (< alt 26))
				    (vector-set! tog alt  (+ 1 (vector-ref tog alt)))
				    ;; macro increment tog0 .. tog25 depending on value alt
				    ;;(toggle-check alt)
				    (when (= 0 alt) (set! tog0 (+ tog0 1)))
				    (when (= 1 alt) (set! tog1 (+ tog1 1)))
				    (when (= 2 alt) (set! tog2 (+ tog2 1)))
				    (when (= 3 alt) (set! tog3 (+ tog3 1)))
				    (when (= 4 alt) (set! tog4 (+ tog4 1)))
				    (when (= 5 alt) (set! tog5 (+ tog5 1)))
				    (when (= 6 alt) (set! tog6 (+ tog6 1)))
				    (when (= 7 alt) (set! tog7 (+ tog7 1)))
				    (when (= 8 alt) (set! tog8 (+ tog8 1)))
				    (when (= 9 alt) (set! tog9 (+ tog9 1)))
				    (when (= 10 alt) (set! tog10 (+ tog10 1)))
				    (when (= 11 alt) (set! tog11 (+ tog11 1)))
				    (when (= 12 alt) (set! tog12 (+ tog12 1)))
				    (when (= 13 alt) (set! tog13 (+ tog13 1)))
				    (when (= 14 alt) (set! tog14 (+ tog14 1)))
				    (when (= 15 alt) (set! tog15 (+ tog15 1)))
				    (when (= 16 alt) (set! tog16 (+ tog16 1)))
				    (when (= 17 alt) (set! tog17 (+ tog17 1)))
				    (when (= 18 alt) (set! tog18 (+ tog18 1)))
				    (when (= 19 alt) (set! tog19 (+ tog19 1)))
				    (when (= 20 alt) (set! tog20 (+ tog20 1)))
				    (when (= 21 alt) (set! tog21 (+ tog21 1)))
				    (when (= 22 alt) (set! tog22 (+ tog22 1)))
				    (when (= 23 alt) (set! tog23 (+ tog23 1)))
				    (when (= 24 alt) (set! tog24 (+ tog24 1)))
				    (when (= 25 alt) (set! tog25 (+ tog25 1)))
				    
				    
				    ;;(format #t "toggled ~a ~%" alt)
				    (begin (,%set! i (,%+ i 1)) (,fnext)))
				   (#t
				    ;; skip toggle outside program
				    (begin (,%set! i (,%+ i 1)) (,fnext))))))
			       ((,%even? (- (,%vector-ref tog ,n) 1)) (,vna))
			       (#t (,vnb)))))
	  (,%vector-set! fvec ,n ,fn)
	  (,%define ,vna (,%lambda ()   (,%set! ,arg (,%+ ,arg 1)) (begin (,%set! i (,%+ i 1)) (,fnext)))) ;; inc
	  (,%define ,vnb (,%lambda ()  (,%set! ,arg (,%- ,arg 1)) (begin (,%set! i (,%+ i 1)) (,fnext)))) ;; dec	      
	  )))))





;; cpy a b  ... no cpy a -2 in the input 
;; cpy a -2  => jnz a -2
;; cpy 1 -2  => jnz a -2  ok
(begin-for-syntax
 (define compile-cpy
   (lambda (n arg1 arg2 rename compare?)
     (let ((fn (string->symbol (format #f "f~a" n)))
	   (togn (string->symbol (format #f "tog~a" n)))
	   (fnext (string->symbol (format #f "f~a" (+ n 1))))
	   (vna (string->symbol (format #f "v~aa" n)))
	   (vnb (string->symbol (format #f "v~ab" n)))
	   (%set! (rename 'set!))
	   (%define (rename 'define))
	   (%lambda (rename 'lambda))
	   (%+ (rename '+))
	   (%- (rename '-))	       
	   (%vector-set! (rename 'vector-set!))
	   (%even? (rename 'even?))
	   (%vector-ref (rename 'vector-ref))
	   (%not (rename 'not))
	   (%zero? (rename 'zero?))
	   (%if (rename 'if)))
       `(begin
	  (,%define ,fn (,%lambda () (,%if (,%even? ,togn ;;(,%vector-ref tog ,n)
						    ) (,vna) (,vnb))))
	  (,%vector-set! fvec ,n ,fn)
	  ;; cpy a b =[ b <- a ]
	  (,%define ,vna (,%lambda ()   (,%set! ,arg2 ,arg1) (begin (,%set! i (,%+ i 1)) (,fnext))))
	  ;; jnz a b	  
	  (,%define ,vnb (,%lambda ()   (,%if ;;(,%not (,%zero? ,arg1))
					 (,%zero? ,arg1)
					 (begin (,%set! i (+ i 1)) (,fnext))
					 (,%set! i (+ i ,arg2))
					    
					    )))
	  )))))

;; (pp (expand* '(gen 0 (cpy a b))))



#|
jnz d -2
=> cpy d -2 which is invalid code
|#
(begin-for-syntax
 (define compile-jnz
   (lambda (n arg1 arg2 rename compare?)
     (let ((fn (string->symbol (format #f "f~a" n)))
	   (togn (string->symbol (format #f "tog~a" n)))
	   (fnext (string->symbol (format #f "f~a" (+ n 1))))
	   (vna (string->symbol (format #f "v~aa" n)))
	   (vnb (string->symbol (format #f "v~ab" n)))
	   (%set! (rename 'set!))
	   (%define (rename 'define))
	   (%lambda (rename 'lambda))
	   (%+ (rename '+))
	   (%- (rename '-))	       
	   (%vector-set! (rename 'vector-set!))
	   (%even? (rename 'even?))
	   (%vector-ref (rename 'vector-ref))
	   (%not (rename 'not))
	   (%zero? (rename 'zero?))
	   (%if (rename 'if)))
       `(begin
	  (,%define ,fn (,%lambda () (,%if (,%even? ,togn
						    ;;(,%vector-ref tog ,n)
						    ) (,vna) (,vnb))))
	  (,%vector-set! fvec ,n ,fn)
	  ;; jnz a b	  
	  (,%define ,vna (,%lambda ()  
				   (,%if ;;(,%not (,%zero? ,arg1))
				    (,%zero? ,arg1)
				    (begin (,%set! i (,%+ i 1)) (,fnext))
				    ,(cond
				      ((integer? arg2)
				       (let ((fgo (string->symbol (format #f "f~a" (+ n arg2)))))
					 `(begin (,%set! i (,%+ i ,arg2))
						 (,fgo))))				       
				      (#t `(,%set! i (,%+ i ,arg2)))))))
					 
	  ;; cpy a b =[ b <- a ]
	  ;; if b is an integer , SKIP 
	  ,(cond
	    ((integer? arg2) `(,%define ,vnb (,%lambda ()   (begin (,%set! i (,%+ i 1)) (,fnext)))))
	    (#t `(,%define ,vnb (,%lambda ()   (,%set! ,arg2 ,arg1) (begin (,%set! i (,%+ i 1)) (,fnext))))))	  
	  )))))



;; (pp (expand* '(gen 25 (jnz c -5))))
;; (pp (expand* '(gen 12 (cpy c d))))

#|

what is compile-tgl supposed to do then ?


|#


(begin-for-syntax (define (abcd? x) (or (eq? x 'a)(eq? x 'b)(eq? x 'c)(eq? x 'd))))

(define-syntax gen
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((n (car (cdr form)))
	   (exp (car (cdr (cdr form)))))
       (cond
	((= (length exp) 2) ;; one arg version
	 (bind (op arg) exp
	       (cond
		((and (eq? op 'dec) (abcd? arg))
		 (compile-dec n arg rename compare?))
		((and (eq? op 'inc) (abcd? arg))
		 (compile-inc n arg rename compare?))
		((and (eq? op 'tgl) (abcd? arg))
		  (compile-tgl n arg rename compare?))		
		(#t (error (format #f "bad gen : I : op[~a] arg[~a] " op arg))))))
	((= (length exp) 3) ;; two arg version
	 (bind (op arg1 arg2) exp
	       (cond
		((eq? op 'cpy) 
		 (compile-cpy n arg1 arg2 rename compare?))
		((eq? op 'jnz) 
		 (compile-jnz n arg1 arg2 rename compare?))
		(#t (error (format #f "bad gen : II : op[~a] arg1[~a] arg2[~a] " op arg1 arg2))))))
	(#t (error (format #f "bad gen : III : exp[~a] " exp))))))))


;;(pp (expand* '(gen 1 (cpy a b))))

;; (pp (expand* '(gen 2 (dec a))))
;; (pp (expand* '(gen 5 (inc a))))
;; (pp (expand* '(gen 7 (inc c))))

;;(pp (expand* '(gen 22 (inc z)))) ;; z not a b c or d
;;(pp (expand* '(gen 2 (tgl a)))) ;; tgl no idea about this yet

;; optimize not out of (jnz a d) -> (if (not (zero? a)) (set! i (+ i d)) (set! i (+ i 1)))))
;; -> (if (zero? a)  (set! i (+ i 1)) (set! i (+ i d)))
;; since we know i = 2 at this point
;; -> (if (zero? a)  ((vector-ref fvec 3)) (set! i (+ 2 d)))
;;                   ^^^ directly execute f3 ()
;;

;; (define toggle
;;   (lambda (expr)
;;     (match expr
;;       (('inc a) `(dec ,a))
;;       ((_ a) `(inc ,a))      
;;       (('jnz a b) `(cpy ,a ,b))
;;       ((_ a b) `(jnz ,a ,b))
;;       ( _ (error "toggle : no match on expr")))))

;; make an emacs save when get kill command ?
;;

;; (define create-procedures
;;   (lambda ()
;;     (let ((i -1))
;;       (map (lambda (x) (set! i (+ i 1))
;; 		   (format #t "~a~%" `(gen ,i ,x) ))
;; 	   input)
;;       #t)))

;; these are macros 
(define create-procedures
  (lambda ()
    #t
    ))

;; for compilation these need to be toplevel ?
(gen 0 (cpy a b))
(gen 1 (dec b))
(gen 2 (cpy a d))
(gen 3 (cpy 0 a))
(gen 4 (cpy b c))
(gen 5 (inc a))
(gen 6 (dec c))
(gen 7 (jnz c -2))
(gen 8 (dec d))
(gen 9 (jnz d -5))
(gen 10 (dec b))
(gen 11 (cpy b c))
(gen 12 (cpy c d))
(gen 13 (dec d))
(gen 14 (inc c))
(gen 15 (jnz d -2))
(gen 16 (tgl c))
(gen 17 (cpy -16 c))
(gen 18 (jnz 1 c))
(gen 19 (cpy 98 c))
(gen 20 (jnz 86 d))
(gen 21 (inc a))
(gen 22 (inc d))
(gen 23 (jnz d -2))
(gen 24 (inc c))
(gen 25 (jnz c -5))




(define reset
  (lambda ()
    (set! a 0)
    (set! b 0)
    (set! c 0)
    (set! d 0)
    (set! i 0)
    (set! tog (make-vector 30 0))
    ;; (set! fvec (make-vector 30 #f))
    (set! f26 (lambda () 'done))
    (create-procedures)
    (set! tog0 0)
    (set! tog1 0)
    (set! tog2 0)
    (set! tog3 0)
    (set! tog4 0)
    (set! tog5 0)
    (set! tog6 0)
    (set! tog7 0)
    (set! tog8 0)
    (set! tog9 0)
    (set! tog10 0)
    (set! tog11 0)
    (set! tog12 0)
    (set! tog13 0)
    (set! tog14 0)
    (set! tog15 0)
    (set! tog16 0)
    (set! tog17 0)
    (set! tog18 0)
    (set! tog19 0)
    (set! tog20 0)
    (set! tog21 0)
    (set! tog22 0)
    (set! tog23 0)
    (set! tog24 0)
    (set! tog25 0)
    (set! tog26 0) ;; redundant 26th
    (set! i 0)))



;; keep running if 
(define run
  (lambda ()
    (letrec ((foo (lambda ()
		    (cond
		     ((or (< i 0) (> i 25)) 'done)
		     (#t (let ((fun (vector-ref fvec i)))
			   ;;(format #t "running procedure ~a ~%" i )
			   (fun)	   
			   (foo)))))))
      (reset)
      (set! a 7) ;; 7 eggs run 1
      (foo)
      (format #t "a[~a] b[~a] c[~a] d[~a]~%" a b c d)
      )))

;; keep running if 
(define run2
  (lambda ()
    (letrec ((foo (lambda ()
		    (cond
		     ((or (< i 0) (> i 25)) 'done)
		     (#t (let ((fun (vector-ref fvec i)))
			   ;; (format #t "running procedure ~a ~%" i )
			   (fun)	   
			   (foo)))))))
      (reset)
      (set! a 12) ;; 12 eggs run 2
      (foo)
      (format #t "a[~a] b[~a] c[~a] d[~a]~%" a b c d)
      )))


;;(pp (expand* '(gen 0 (cpy a b))))

;;(run)


#|

> ,t (run)
a[13468] b[1] c[0] d[0]
0.03s CPU time, 20896/4241 mutations (total/tracked), 0/710 GCs (major/minor), maximum live heap: 1.19 MiB

|#


;;(run)

(run2)


#|
a[479010028] b[1] c[0] d[0]

real	2m30.868s
user	2m30.173s
sys	0m0.680s

FNEXT optimisation when know where need to go to yields a 30 second improvement .
--------------------
a[479010028] b[1] c[0] d[0]

real	1m57.456s
user	1m57.015s
sys	0m0.429s

NOT optimisation
-----------------
if (not (zero? X)) A B  -> zero? X then B else A

VECTOR-tog optiomisation
-------------------------
rather than a toggle vector - have individual variables , skip an array lookup
tog0 0

TOGGLE optimisation
---------------------
now put load onto toggle to set both tog vector value (vector-ref tog i)
and individual toggle value tog0 .. tog26

> time ./optimize
a[479010028] b[1] c[0] d[0]

real	1m20.563s
user	1m20.288s
sys	0m0.264s

saved another 37 seconds 
-----------------------------------

JNZ A -2
if know offset we may jump to we can update i  such as i = i - 2 , jump to fgo
example
i=9 : jnz d -5 :  on branch when d non-zero  i = i - 5 , 9 - 5 is 4 so fgo will be f4 hence (f4)

> time ./optimize
a[479010028] b[1] c[0] d[0]

real	1m9.573s
user	1m9.394s
sys	0m0.173s

shaved another 10 seconds 
--------------------------------------

compiling with -O5 unsafe everything and kitchen sink

terry@debian:~/code/advent-of-code/advent-of-code-2016/day23/chicken$ csc -O5 optimize.scm
terry@debian:~/code/advent-of-code/advent-of-code-2016/day23/chicken$ time ./optimize 
a[479010028] b[1] c[0] d[0]

real	0m22.681s
user	0m22.678s
sys	0m0.001s

more than halves the

----------------------------------------



|#


