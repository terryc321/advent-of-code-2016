

(import scheme)
(import (chicken format))
(import (chicken pretty-print))
(import srfi-1)
(import matchable)


#|
recap asm bunny code
four registers a b c d
all initially zero i suppose

    cpy x y copies x (either an integer or the value of a register) into register y.
    inc x increases the value of register x by one.
    dec x decreases the value of register x by one.
    jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.
 
|#

(define example
  (lambda ()
    (list->vector
     `(
       (cpy 2 a)
       (tgl a)
       (tgl a)
       (tgl a)
       (cpy 1 a)
       (dec a)
       (dec a)))))

(define input
  (lambda ()
    (list->vector
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
       (jnz c -5)))))

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

	  
(define toggle!
  (lambda (ins)
    (match ins
      (('inc a) `(dec ,a))
      ((_ a) `(inc ,a))
      (('jnz x y) `(cpy ,x ,y))
      ((_ x y) `(jnz ,x ,y))
      ( _ (error "unknown toggle")))))

(define exec
  (lambda (p)
    (let* ((prog (copy-vec p))
	   (reg-a 7)
	   (reg-b 0)
	   (reg-c 0)
	   (reg-d 0)
	   (lim (- (vector-length prog) 1)))
      (letrec ((foo (lambda (i)
		      ;; (format #t "~%")
		      ;; (pp prog)
		      ;; (format #t "~%----------------~%")
		      
		      (cond
		       ((> i lim) 'done)
		       ((< i 0) 'done)
		       (#t (let ((instr (vector-ref prog i)))
			     ;;(format #t "instr ~a => ~a ~%" i instr)
			     (match instr
			       
			       (('inc 'a)
				(set! reg-a (+ 1 reg-a))
				(foo (+ i 1)))
			       
			       (('dec 'a)
				(set! reg-a (- reg-a 1))
				(foo (+ i 1)))
			       (('dec 'b)
				(set! reg-b (- reg-b 1))
				(foo (+ i 1)))
			       (('dec 'c)
				(set! reg-c (- reg-c 1))
				(foo (+ i 1)))			       
			       (('dec 'd)
				(set! reg-d (- reg-d 1))
				(foo (+ i 1)))
			       
			       (('inc 'a)
				(set! reg-a (+ reg-a 1))
				(foo (+ i 1)))
			       (('inc 'b)
				(set! reg-b (+ reg-b 1))
				(foo (+ i 1)))
			       (('inc 'c)
				(set! reg-c (+ reg-c 1))
				(foo (+ i 1)))			       
			       (('inc 'd)
				(set! reg-d (+ reg-d 1))
				(foo (+ i 1)))		       
			       
			       (('tgl 'a)
				(let ((val (vector-ref prog (+ i reg-a))))
				  (vector-set! prog (+ i reg-a) (toggle! val))
				  ;;(vector-set! prog reg-a (toggle! val))
				  (foo (+ i 1))))

			       (('jnz 'c n) 
				(cond
				 ((and (integer? reg-c) (zero? reg-c)) ;; nop 
				  (foo (+ i 1)))				 
				 ((integer? n)
				  (cond
				   ((< reg-c 0) (foo (- i n)))
				   (#t (foo (+ i n)))))
				 (#t
				  (format #t "JNZ FAK me..~%"))))

			       (('jnz 'd n) 
				(cond
				 ((and (integer? reg-d) (zero? reg-d)) ;; nop 
				  (foo (+ i 1)))				 
				 ((integer? n)
				  (cond
				   ((< reg-d 0) (foo (- i n)))
				   (#t (foo (+ i n)))))
				 (#t
				  (format #t "JNZ FAK me..~%"))))

			       
			       (('jnz n 'a) 
				(cond
				 ((and (integer? n) (zero? n)) ;; nop 
				  (foo (+ i 1)))				 
				 ((integer? n)
				  (cond
				   ((< n 0) (foo (- i reg-a)))
				   (#t (foo (+ i reg-a)))))
				 (#t
				  (format #t "JNZ FAK me..~%"))))

			       (('jnz n 'c) 
				(cond
				 ((and (integer? n) (zero? n)) ;; nop 
				  (foo (+ i 1)))				 
				 ((integer? n)
				  (cond
				   ((< n 0) (foo (- i reg-c)))
				   (#t (foo (+ i reg-c)))))
				 (#t
				  (format #t "JNZ c FAK me..~%"))))

			       (('cpy 'a 'b)
				(set! reg-b reg-a)
				(foo (+ i 1)))
			       (('cpy 'a 'd)
				(set! reg-a reg-d)
				(foo (+ i 1)))
			       (('cpy 'b 'c)
				(set! reg-c reg-b)
				(foo (+ i 1)))
			       (('cpy 'c 'd)
				(set! reg-d reg-c)
				(foo (+ i 1)))

			       (('cpy n 'a)
				(if (not (integer? n)) (error "cpy n 'a"))
				(set! reg-a n)
				(foo (+ i 1)))

			       (('cpy n 'b)
				(if (not (integer? n)) (error "cpy n 'b"))
				(set! reg-a n)
				(foo (+ i 1)))

			       (('cpy n 'c)
				(if (not (integer? n)) (error "cpy n 'c"))
				(set! reg-c n)
				(foo (+ i 1)))

			       (('cpy n 'd)
				(if (not (integer? n)) (error "cpy n 'd"))
				(set! reg-d n)
				(foo (+ i 1)))
			       			       
			       (('cpy n 'a)
				(if (not (integer? n)) (error "cpy n 'a"))
				(cond
				 ((integer? n)
				  (set! reg-a n)
				  (foo (+ i 1))
				  )
				 ((symbol? n)
				  (format #t "FAK me..~%"))))
			       
			       ( _ (format #t "unrecognised expression ~a ~%" instr)))))))))
			     
	(foo 0)
	(format #t "Registers a-d :  ~a ~a ~a ~a ~%" reg-a reg-b reg-c reg-d)))))


(exec (input))


(format #t "what ? ~%")



      
    


