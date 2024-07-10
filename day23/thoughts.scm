#|
some thoughts.txt
----------------------

tgl x toggles the instruction x away (pointing at instructions like jnz does: positive means forward; negative means backward):

For one-argument instructions,
inc becomes dec,
and all other one-argument instructions become inc.

For two-argument instructions, jnz becomes cpy, and all other two-instructions become jnz.
The arguments of a toggled instruction are not affected.
If an attempt is made to toggle an instruction outside the program, nothing happens.
If toggling produces an invalid instruction (like cpy 1 2) and an attempt is later made to execute that instruction, skip it instead.
If tgl toggles itself (for example, if a is 0, tgl a would target itself and become inc a), the resulting instruction is not executed until the next time it is reached.

vector of toggles ?

look at each instruction and see how it toggles

toggle ( i >= 0 and i < program.length )
otherwise toggle is ignored and next instruction is fired

note toggle procedure affecting tog[n] is independent on firing of v[n]
meaning tog[n] can be incremented , but v[n] is not fired directly after

-----------------------------------------------
one argument instruction - First Toggle 
inc -> dec
* anything else * ->  inc

second Toggle
dec -> inc
inc -> dec

tog[0] = 0
v[0] = if tog[0] == 0 then ... else if 
-----------------------------------------------
two argument instruction - First toggle
jnz -> cpy
*anything* -> jnz

invalid instructions are skipped
cpy 1 2


-----------------------
0 cpy a b
1 dec b
2 cpy a d
3 cpy 0 a
4 cpy b c
5 inc a
6 dec c
7 jnz c -2
8 dec d
9 jnz d -5
10 dec b
11  cpy b c
12  cpy c d
13  dec d
14  inc c
15  jnz d -2
16  tgl c
17  cpy -16 c
18  jnz 1 c
19  cpy 98 c
20  jnz 86 d
21  inc a
22  inc d
23  jnz d -2
24  inc c
25  jnz c -5
-------------------

|#


(import (chicken format))
(import (chicken pretty-print))
(import matchable)
(import procedural-macros)
(import expand-full)


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
(import-for-syntax (only (chicken format) format))
(import-for-syntax (only bindings bind))

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
	  (,%define ,fn (,%lambda () (,%if (,%even? (,%vector-ref tog ,n)) (,vna) (,vnb))))
	  (,%vector-set! fvec ,n ,fn)
	  (,%define ,vna (,%lambda () (bug ',vna) (,%set! ,arg (,%+ ,arg 1)) (begin (,%set! i (,%+ i 1)) "(,fnext)"))) ;; inc b
	  (,%define ,vnb (,%lambda ()  (bug ',vnb) (,%set! ,arg (,%- ,arg 1)) (begin (,%set! i (,%+ i 1)) "(,fnext)"))) ;; dec b	      
	  )))))


(begin-for-syntax
 (define compile-dec
   (lambda (n arg rename compare?)
     (let ((fn (string->symbol (format #f "f~a" n)))
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
	  (,%define ,fn (,%lambda () (,%if (,%even? (,%vector-ref tog ,n)) (,vna) (,vnb))))
	  (,%vector-set! fvec ,n ,fn)
	  (,%define ,vna (,%lambda () (bug ',vna) (,%set! ,arg (,%- ,arg 1)) (begin (,%set! i (,%+ i 1)) "(,fnext)"))) ;; dec b
	  (,%define ,vnb (,%lambda ()  (bug ',vnb) (,%set! ,arg (,%+ ,arg 1)) (begin (,%set! i (,%+ i 1)) "(,fnext)"))) ;; inc b	      
	  )))))


#|
;; tgl x
;;  => inc => dec => inc repeat infinitely
|#
(begin-for-syntax
 (define compile-tgl
   (lambda (n arg rename compare?)
     (let ((fn (string->symbol (format #f "f~a" n)))
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
			       ((,%zero? (,%vector-ref tog ,n))
				(let ((alt (+ i ,arg)))
				  (,%cond
				   ((,%and (>= alt 0) (< alt 26))
				    (vector-set! tog alt  (+ 1 (vector-ref tog alt)))
				    ;;(format #t "toggled ~a ~%" alt)
				    (begin (,%set! i (,%+ i 1)) "(,fnext)"))
				   (#t
				    ;; skip toggle outside program
				    (begin (,%set! i (,%+ i 1)) "(,fnext)")))))
			       ((,%even? (- (,%vector-ref tog ,n) 1)) (,vna))
			       (#t (,vnb)))))
	  (,%vector-set! fvec ,n ,fn)
	  (,%define ,vna (,%lambda ()  (bug ',vna) (,%set! ,arg (,%+ ,arg 1)) (begin (,%set! i (,%+ i 1)) "(,fnext)"))) ;; inc
	  (,%define ,vnb (,%lambda ()  (bug ',vnb) (,%set! ,arg (,%- ,arg 1)) (begin (,%set! i (,%+ i 1)) "(,fnext)"))) ;; dec	      
	  )))))




;; cpy a b  ... no cpy a -2 in the input 
;; cpy a -2  => jnz a -2
;; cpy 1 -2  => jnz a -2  ok
(begin-for-syntax
 (define compile-cpy
   (lambda (n arg1 arg2 rename compare?)
     (let ((fn (string->symbol (format #f "f~a" n)))
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
	  (,%define ,fn (,%lambda () (,%if (,%even? (,%vector-ref tog ,n)) (,vna) (,vnb))))
	  (,%vector-set! fvec ,n ,fn)
	  ;; cpy a b =[ b <- a ]
	  (,%define ,vna (,%lambda ()  (bug ',vna) (,%set! ,arg2 ,arg1) (begin (,%set! i (,%+ i 1)) "(,fnext)")))
	  ;; jnz a b	  
	  (,%define ,vnb (,%lambda ()  (bug ',vnb) (,%if (,%not (,%zero? ,arg1)) 
					    (,%set! i (+ i ,arg2))
					    (begin (,%set! i (+ i 1)) "(,fnext)"))))
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
	  (,%define ,fn (,%lambda () (,%if (,%even? (,%vector-ref tog ,n)) (,vna) (,vnb))))
	  (,%vector-set! fvec ,n ,fn)
	  ;; jnz a b	  
	  (,%define ,vna (,%lambda ()  (bug ',vna)
				   (,%if (,%not (,%zero? ,arg1)) 
					 (,%set! i (+ i ,arg2))
					 (begin (,%set! i (,%+ i 1)) "(,fnext)"))))
	  ;; cpy a b =[ b <- a ]
	  ;; if b is an integer , SKIP 
	  ,(cond
	    ((integer? arg2) `(,%define ,vnb (,%lambda ()  (bug ',vnb) (begin (,%set! i (,%+ i 1)) "(,fnext)"))))
	    (#t `(,%define ,vnb (,%lambda ()  (bug ',vnb) (,%set! ,arg2 ,arg1) (begin (,%set! i (,%+ i 1)) "(,fnext)")))))	  
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

(define toggle
  (lambda (expr)
    (match expr
      (('inc a) `(dec ,a))
      ((_ a) `(inc ,a))      
      (('jnz a b) `(cpy ,a ,b))
      ((_ a b) `(jnz ,a ,b))
      ( _ (error "toggle : no match on expr")))))


(define create-create-procedures
  (lambda ()
    (let ((i -1)
	  (xs '()))
      (map (lambda (x) (set! i (+ i 1))
		   (set! xs (cons `(gen ,i ,x) xs)))
	   input)
      (pp (reverse xs)))))




(define create-procedures
  (lambda ()
    (let ((i -1))
      (map (lambda (x) (set! i (+ i 1))
		   (eval `(gen ,i ,x) (interaction-environment)))
	   input))))




(define reset
  (lambda ()
    (set! a 0)
    (set! b 0)
    (set! c 0)
    (set! d 0)
    (set! i 0)
    (set! tog (make-vector 30 0))
    (set! fvec (make-vector 30 #f))
    (set! f26 (lambda () 'done))
    (create-procedures)
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
			   ;;(format #t "running procedure ~a ~%" i )
			   (fun)	   
			   (foo)))))))
      (reset)
      (set! a 12) ;; 12 eggs run 2
      (foo)
      (format #t "a[~a] b[~a] c[~a] d[~a]~%" a b c d)
      )))


;;(pp (expand* '(gen 0 (cpy a b))))

#|

> ,t (run)
a[13468] b[1] c[0] d[0]
0.03s CPU time, 20896/4241 mutations (total/tracked), 0/710 GCs (major/minor), maximum live heap: 1.19 MiB

|#

(run2)





