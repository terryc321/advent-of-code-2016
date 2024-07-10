

(import scheme)
(import (chicken format))
(import (chicken pretty-print))
(import srfi-1)
(import matchable)
(import (chicken syntax))
(import procedural-macros)

;; (inc! a)
;; (inc! a 2)
(define-syntax inc!
  (er-macro-transformer
   (lambda (exp rename compare)
     (cond ;; (inc! a) -> (set! a (+ a 1)) ;; default to add 1
      ((null? (cdr (cdr exp)))
       (let ((var (car (cdr exp)))
	     (%set! (rename 'set!))
	     (%+ (rename '+)))
	 `(,%set! ,var (,%+ ,var 1))))
      (#t ;; (inc! a 3) -> (set! a (+ a 3)) ;; add N
       (let ((var (car (cdr exp)))
	     (n (car (cdr (cdr exp))))
	     (%set! (rename 'set!))
	     (%+ (rename '+)))
	 `(,%set! ,var (,%+ ,var ,n))))))))

;; (let ((a 1))  (inc! a)  a)
;; (let ((a 1))  (inc! a 3)  a)
;; -----------------------------------------------------------------------------

#|
recap asm bunny code
four registers a b c d
all initially zero i suppose

    cpy x y copies x (either an integer or the value of a register) into register y.
    inc x increases the value of register x by one.
    dec x decreases the value of register x by one.
    jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.

tgl x toggles the instruction x away
(pointing at instructions like jnz does: positive means forward; negative means backward):

    For one-argument instructions, inc becomes dec, and all other one-argument instructions become inc.
    For two-argument instructions, jnz becomes cpy, and all other two-instructions become jnz.
    The arguments of a toggled instruction are not affected.
    If an attempt is made to toggle an instruction outside the program, nothing happens.
    If toggling produces an invalid instruction (like cpy 1 2) and an attempt is later made to execute that instruction, skip it instead.
    If tgl toggles itself (for example, if a is 0, tgl a would target itself and become inc a), the resulting instruction is not executed until the next time it is reached.


|#



(define example
  (lambda ()
     `(
       (cpy 2 a)
       (tgl a)
       (tgl a)
       (tgl a)
       (cpy 1 a)
       (dec a)
       (dec a))))

(define input
  (lambda ()
     `((cpy a b)
       (dec b)
       (cpy a d)
       (cpy 0 a)
       (cpy b c)
       (inc a)
       (dec c)
       (jnz c -2)
       (dec d)
       (jnz d -5)
       (dec b)
       (cpy b c)
       (cpy c d)
       (dec d)
       (inc c)
       (jnz d -2)
       (tgl c)
       (cpy -16 c)
       (jnz 1 c)
       (cpy 98 c)
       (jnz 86 d)
       (inc a)
       (inc d)
       (jnz d -2)
       (inc c)
       (jnz c -5))))

;; (define f (input))
;; (set-car! f `(cpy b c))
(define copy-vec
  (lambda (p)
    (let* ((lim (- (vector-length p) 1))
	   (res (make-vector (+ lim 1))))
      (letrec ((foo (lambda (i)
		      (cond
		       ((> i lim) #f)
		       (#t 
			(let ((val (vector-ref p i)))
			  (vector-set! res i val)
			  (foo (+ i 1))))))))
	(foo 0)
	res))))

	  
(define compile-toggle!
  (lambda (ins)
    (match ins
      (('inc a) `(dec ,a))
      ((_ a) `(inc ,a))
      (('jnz x y) `(cpy ,x ,y))
      ((_ x y) `(jnz ,x ,y))
      ( _ (error "unknown toggle")))))


;; -------------------------------------------------------------------------------
;; (cpy a b) a,b both symbols
;; (cpy 3 b) 
(define compile-cpy
  (lambda (a b)
    (cond
     ((and (symbol? b)) `(lambda () (begin (set! ,b ,a) (inc! i))))
     ;;((and (integer? a) (symbol? b)) `(set! ,b ,a))
     (#t (error (list "compile-cpy" a b))))))

;; (jnz c 2) 
(define compile-jnz
  (lambda (a b)
    (cond
     ((and (symbol? a)) `(lambda () (if (zero? ,a) (inc! i) (inc! i ,b))))
     ((and (integer? a)) (if (zero? a)
			     `(lambda () (inc! i))
			     `(lambda () (inc! i ,b))))
     (#t (error (list "compile-jnz" a b))))))


;; implicit index i
;; (tgl a) a is a symbol 
(define compile-tgl
  (lambda (a)
    (cond
     ((and (symbol? a)) `(lambda () (begin (runtime-toggle! (+ ,a i)) (inc! i))))
     (#t (error (list "compile-tgl" a))))))


;; (dec a) a is a symbol 
(define compile-dec
  (lambda (a)
    (cond
     ((and (symbol? a)) `(lambda () (begin (set! ,a (- ,a 1)) (inc! i))))
     (#t (error (list "compile-dec" a))))))


(define compile-inc
  (lambda (a)
    (cond
     ((and (symbol? a)) `(lambda () (begin (set! ,a (+ ,a 1)) (set! i (+ i 1)))))
     (#t (error (list "compile-inc" a))))))


(define compile-expr
  (lambda (e)
    (match e
      (('cpy a b) (compile-cpy a b))
      (('jnz a b) (compile-jnz a b))      
      (('tgl a) (compile-tgl a))
      (('dec a) (compile-dec a))
      (('inc a) (compile-inc a))      
      ( _ (error (list "compile-expr" e))))))


;;(pp (map compile-expr (example)))

;;(exec (input))

;; ----------------------------------------------------
(define a 0)
(define b 0)
(define c 0)
(define d 0)
(define i 0)

(define reset! (lambda ()
		 (set! a 0)
		 (set! b 0)
		 (set! c 0)
		 (set! d 0)
		 (set! i 0)))


(define code (input))
(define prog (list->vector ;;(map (lambda (expr) (eval expr (interaction-environment)))
	      (map compile-expr code)))


(define run
  (lambda ()
    (reset!)
    (let ((lim (vector-length prog)))
      (letrec ((next (lambda ()
		       (cond
			((< i 0) 'pass-begin-tape)
			((>= i lim) 'past-end-tape)
			(#t
			 (let ((cmd (vector-ref prog i)))
			   (cmd)
			   (next)))))))
	(next)))))











      
    


