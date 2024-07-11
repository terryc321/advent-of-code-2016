

#|

a b c d
set a to lowest value such output gives alternating pattern 0 , 1 , 0 , 1 ...

|#

(import (chicken format))
(import (chicken pretty-print))
(import matchable)
(import procedural-macros)
(import expand-full)

(import-for-syntax (only (chicken format) format))
(import-for-syntax (only bindings bind))

;; tog initialized to all zeros at start
(define original-a 0)
(define a 0)
(define b 0)
(define c 0)
(define d 0)
(define i 0)  
(define fvec (make-vector 40 #f))
(define f30 (lambda () 'done))
(define out #f)

(define input '(
		(cpy a d)
		(cpy 7 c)
		(cpy 365 b)
		(inc d)
		(dec b)
		(jnz b -2)
		(dec c)
		(jnz c -5)
		(cpy d a)
		(jnz 0 0)
		(cpy a b)
		(cpy 0 a)
		(cpy 2 c)
		(jnz b 2)
		(jnz 1 6)
		(dec b)
		(dec c)
		(jnz c -4)
		(inc a)
		(jnz 1 -7)
		(cpy 2 b)
		(jnz c 2)
		(jnz 1 4)
		(dec b)
		(dec c)
		(jnz 1 -4)
		(jnz 0 0)
		(out b)
		(jnz a -19)
		(jnz 1 -21)
		))


;; (define-macro (swap x y)
;;   (let ((tmp (gensym 'tmp)))
;;     `(begin (set! ,tmp ,x)
;; 	    (set! ,x ,y)
;; 	    (set! ,y ,tmp))))

;; (pp (expand* '(swap a b)))
;; 
;; (let ((a 1)(b 2))
;;   (format #t "before => ~a ~%" (list a b))
;;   (swap a b)
;;   (format #t "after => ~a ~%" (list a b)))


;; -- can we define macro directly but lost where we are in program for expansion
;; (define-macro (inc x) 
;;   (let ((fn (string->symbol (format #f "f~a" n)))
;; 	(fnext (string->symbol (format #f "f~a" (+ n 1))))
;; 	(vna (string->symbol (format #f "v~aa" n)))
;; 	(vnb (string->symbol (format #f "v~ab" n))))
;;     `(begin
;;        (define ,fn (lambda ()
;; 		     (set! ,x (+ ,x 1))
;; 		     (set! i (+ i 1))
;; 		     (,fnext)))       
;;        ;;(vector-set! fvec ,n ,fn)
;;        )))

(begin-for-syntax
 (define compile-inc
   (lambda (n arg rename compare?)
     (let ((fn (string->symbol (format #f "f~a" n)))
	   (fnext (string->symbol (format #f "f~a" (+ n 1))))
	   (vna (string->symbol (format #f "v~aa" n)))
	   (vnb (string->symbol (format #f "v~ab" n))))
       `(begin
	  (define ,fn (lambda (exit)
			(set! ,arg (+ ,arg 1))
			(set! i (+ i 1))
			(,fnext exit)))       
	  (vector-set! fvec ,n ,fn)
	  )))))


(begin-for-syntax
 (define compile-dec
   (lambda (n arg rename compare?)
     (let ((fn (string->symbol (format #f "f~a" n)))
	   (fnext (string->symbol (format #f "f~a" (+ n 1))))
	   (vna (string->symbol (format #f "v~aa" n)))
	   (vnb (string->symbol (format #f "v~ab" n))))
       `(begin
	  (define ,fn (lambda (exit)
			(set! ,arg (- ,arg 1))
			(set! i (+ i 1))
			(,fnext exit)))
	  (vector-set! fvec ,n ,fn)
	  )))))

#|
uses out object
if out checker is probed too many times and still all okay then it aborts with #true
if out checker finds fault , aborts with #false
otherwise program keeps generating more results
|#
(begin-for-syntax
 (define compile-out
   (lambda (n arg rename compare?)
     (let ((fn (string->symbol (format #f "f~a" n)))
	   (fnext (string->symbol (format #f "f~a" (+ n 1))))
	   (vna (string->symbol (format #f "v~aa" n)))
	   (vnb (string->symbol (format #f "v~ab" n))))
       `(begin
	  (define ,fn (lambda (exit)
			(format #t "~a " ,arg)
			(let ((check (out 'check ,arg)))
			  (cond
			   ((and (out 'okay? #f) (> (out 'count #f) 50))
			    (exit #t))
			   ((not (out 'okay? #f))
			    (format #t "nope ~%")
			    (exit #f)))
			(set! i (+ i 1))
			(,fnext exit))))
	  (vector-set! fvec ,n ,fn)
	  )))))



;; (defmacro (dec x)
;;   (let ((fn (string->symbol (format #f "f~a" n)))
;; 	(fnext (string->symbol (format #f "f~a" (+ n 1))))
;; 	(vna (string->symbol (format #f "v~aa" n)))
;; 	(vnb (string->symbol (format #f "v~ab" n))))
;;     `(begin
;;        (define ,fn (lambda ()
;; 		     (set! ,x (- ,x 1))
;; 		     (set! i (+ i 1))
;; 		     (,fnext)))
;;        ;;(vector-set! fvec ,n ,fn)
;;        )))


;; cpy a b  ... no cpy a -2 in the input 
;; cpy a -2  => jnz a -2
;; cpy 1 -2  => jnz a -2  ok
(begin-for-syntax
 (define compile-cpy
   (lambda (n arg1 arg2 rename compare?)
     (let ((fn (string->symbol (format #f "f~a" n)))
	   (fnext (string->symbol (format #f "f~a" (+ n 1))))
	   (vna (string->symbol (format #f "v~aa" n)))
	   (vnb (string->symbol (format #f "v~ab" n))))
       `(begin
	  (define ,fn (lambda (exit)
			(set! ,arg2 ,arg1)
			(set! i (+ i 1))
			(,fnext exit)))
	  (vector-set! fvec ,n ,fn))))))

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
	   (vnb (string->symbol (format #f "v~ab" n))))
       `(begin
	  (define ,fn (lambda (exit) (cond
				  ((not (zero? ,arg1))  (set! i (+ i ,arg2)))
				  (#t (set! i (+ i 1))
				      (,fnext exit)))))
	  (vector-set! fvec ,n ,fn)
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
		((and (eq? op 'out) (abcd? arg))
		 (compile-out n arg rename compare?))		
		((and (eq? op 'inc) (abcd? arg))
		 (compile-inc n arg rename compare?))
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




(define create-procedures
  (lambda ()
    (let ((i -1))
      (map (lambda (x) (set! i (+ i 1))
		   (eval `(gen ,i ,x) (interaction-environment)))
	   input))))

(define show-procedures
  (lambda ()
    (let ((i -1))
      (map (lambda (x) (set! i (+ i 1))
		   (format #t "~a -> ~a ~%" i x))
	   input))))




(define reset
  (lambda ()
    (set! a 0)
    (set! b 0)
    (set! c 0)
    (set! d 0)
    (set! i 0)
    ;;(set! fvec (make-vector 40 #f))
    (create-procedures)
    (set! i 0)))


(define make-out-checker
    (lambda ()
      (let ((expect 0)
	    (count 0)
	    (all-ok #t))
      (lambda (op arg)
	(cond
	 ((eq? op 'check)
	  (cond
	   ((= arg expect)
	    (set! expect (- 1 expect))
	    (set! count (+ count 1))
	    #t)
	   (#t
	    (set! all-ok #f)
	    (set! count (+ count 1))
	    #f)))	 
	 ((and (eq? op 'count)) count)
	 ((and (eq? op 'okay?)) all-ok)	 
	 (#t (error "out-checker unknown op : ~a ~%" op)))))))



;; (set! out (make-out-checker))
;; (out 'check 3) => #t or #f
;; (out 'count _ ) => number times counted



;; keep running if 
(define run
  (lambda ()
    (let ((len-input (length input))
	  (solution 0)
	  (exit #f))
      (letrec ((foo (lambda ()
		      (cond
		       ((or (< i 0) (> i (- len-input 1))) 'done)
		       (#t (let ((fun (vector-ref fvec i)))
			     ;;(format #t "running procedure ~a ~%" i )
			     (fun exit)	   
			     (foo)))))))
	(reset)
	;;(set! a 7) ;; 7 eggs run 1
	(call/cc (lambda (found)
	(letrec ((bar (lambda (i)
			(format #t "trying a = ~a ~%" i)
			(reset)
			(set! a i)
			;; introduced original-a as by time reach solution below
			;; original-a has been corrupted by running the actual program
			;; get 0 1 0 1 0 1 output but
			;; original a is 175
			;; at output corrupted a is 341 
			(set! original-a a)
			(set! out (make-out-checker))
			(let ((val (call/cc (lambda (cont)
					      (set! exit cont)		       		 
					      (foo)
					      ))))
			  (when val
			    (set! solution original-a)
			    (format #t "corrupted a = ~a ~%" a)
			    (found val)))
			(bar (+ i 1)))))
	  (bar 0))))
	(format #t "proposed solution ~a ~%" solution)))))

	
#|

> (run)

....
....
trying a = 169 
0 0 nope 
trying a = 170 
1 nope 
trying a = 171 
0 1 1 nope 
trying a = 172 
1 nope 
trying a = 173 
0 0 nope 
trying a = 174 
1 nope 
trying a = 175 
0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 proposed solution 341 

1.616s CPU time, 0.006s GC time (major), 1252809/273905 mutations (total/tracked), 12/33906 GCs (major/minor), maximum live heap: 991.99 KiB



|#



