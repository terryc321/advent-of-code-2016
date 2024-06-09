

(import (chicken format))
(import (chicken syntax))
(import (chicken pretty-print))
(import expand-full)

;;(import-for-syntax gen)
;;(require-for-syntax gen)


#|

puzzle input
The fourth floor contains nothing relevant.
The third floor contains a thulium-compatible microchip.
The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.

E the elevator
P PG PM plutonium
R RG RM ruthenium
S SG SM strontium
T TG TM thulium 

4  .  .  .  .  .  .    .  .      .   . 
3  .  .  .  .  .  .    .  .      .   TM
2 CG CM  .  .  RG RM  .   .     TG   .
1 E  .   PG PM  .  .  SG SM      .   .  

'((e pg pm sg sm) (cg cm rg rm tg) (tm) ())

must have atleast one generator or microchip to power elevator
can take atmost two generators or microchips in any combination ,
two generators
two microchips
one generator and one microchip

if microchip and generator are on same floor ok
if microchip is on floor with different generator and not its own generator then microchip is fried

|#

(define (generators) '(pg sg cg rg tg))
(define (microchips) '(pm sm cm rm tm))
(define (elevator) '(e))
(define (initial) '((e pg pm sg sm) (cg cm rg rm tg) (tm) ()))

;; pass along state of search - let e be 1 = first floor , 2 = end floor , 3 = third floor , 4 = fourth

(define e 1)
(define pg 1)
(define pm 1)
(define sg 1)
(define sm 1)

(define cg 2)
(define cm 2)
(define rg 2)
(define rm 2)
(define tg 2)

(define tm 3)

;; fried when microchip is on same level as another generator 
(define (any-fried? e pg pm sg sm cg cm rg rm tg tm)
  (or (and (not (= pm pg)) (member pm (list    sg cg rg tg)))
      (and (not (= sm sg)) (member sm (list pg    cg rg tg)))
      (and (not (= cm cg)) (member cm (list pg sg    rg tg)))
      (and (not (= rm rg)) (member rm (list pg sg cg    tg)))
      (and (not (= tm tg)) (member tm (list pg sg cg rg   )))))

;; complete when everything on 4th floor
(define (completed? e pg pm sg sm cg cm rg rm tg tm)
  (equal? (list e pg pm sg sm cg cm rg rm tg tm)
	  '(    4  4 4  4  4   4  4  4  4  4  4)))


;; begin-for-syntax allows (gen) to be defined at macro expansion time ??
(begin-for-syntax (define (gen)
  (letrec ((syms '(pg pm sg sm cg cm rg rm tg rm))
	   (res '())
	   (add-clause (lambda (x) (set! res (cons x res))))
	   (up-1 (lambda (sym floor)
		   (add-clause
		    `(when (and (= e ,floor) (= ,sym ,floor))
		       (let ((,sym (+ ,sym 1))
			     (s (cons '(moved ,sym up one floor) s)))
			 (let ((new-state (list (+ ,floor 1) pg pm sg sm cg cm rg rm tg tm)))
			   (if (not (member new-state s2))
			       (let ((s2 (cons new-state s2)))
				 (foo (+ ,floor 1) pg pm sg sm cg cm rg rm tg tm s s2)))))))))
	   (down-1 (lambda (sym floor)
		     (add-clause
		      `(when (and (= e ,floor) (= ,sym ,floor))
			 (let ((,sym (- ,sym 1))
			       (s (cons '(moved ,sym down one floor) s)))
			   (let ((new-state (list (+ ,floor 1) pg pm sg sm cg cm rg rm tg tm)))
			     (if (not (member new-state s2))
				 (let ((s2 (cons new-state s2)))
				   (foo (- ,floor 1) pg pm sg sm cg cm rg rm tg tm s s2)))))))))
	   (up-2 (lambda (sym sym2 floor)
		   (when (not (eq? sym sym2))
		   (add-clause
			   `(when (and (= e ,floor) (= ,sym ,floor) (= ,sym2 ,floor))
			      (let ((,sym (+ ,sym 1))
				    (,sym2 (+ ,sym2 1))
				    (s (cons '(moved ,sym and ,sym2 up one floor) s)))
				(let ((new-state (list (+ ,floor 1) pg pm sg sm cg cm rg rm tg tm)))
				  (if (not (member new-state s2))
				      (let ((s2 (cons new-state s2)))
					(foo (+ ,floor 1) pg pm sg sm cg cm rg rm tg tm s s2))))))))))
	   (down-2 (lambda (sym sym2 floor)
		     (when (not (eq? sym sym2))		     
		     (add-clause 
			     `(when (and (= e ,floor) (= ,sym ,floor) (= ,sym2 ,floor))
				(let ((,sym (- ,sym 1))
				      (,sym2 (- ,sym2 1))
				      (s (cons '(moved ,sym and ,sym2 down one floor) s)))
				  (let ((new-state (list (+ ,floor 1) pg pm sg sm cg cm rg rm tg tm)))
				    (if (not (member new-state s2))
					(let ((s2 (cons new-state s2)))
					  (foo (- ,floor 1) pg pm sg sm cg cm rg rm tg tm s s2))))))))))
	   (foo2 (lambda (xs floor)
		   (cond
		    ((> floor 4) #f)
		    ((null? xs) (foo2 syms (+ floor 1)))
		    (#t
		     (let ((sym (car xs)))
		       (cond
			((= floor 1) ; only up
			 (up-1 sym floor))
			((= floor 4) ; only down
			 (down-1 sym floor))
			((or (= floor 2)(= floor 3)) ;; up or down
			 (up-1 sym floor)
			 (down-1 sym floor))
			(#t (error "gen")))
		       (foo2 (cdr xs) floor))))))
	   (foo3 (lambda (xs floor)
		   (cond
		    ((> floor 4) #f)
		    ((null? xs) (foo3 syms (+ floor 1)))
		    (#t
		     (let ((sym (car xs)))
		       (foo4 (cdr xs) floor sym)
		       (foo3 (cdr xs) floor))))))
	   (foo4 (lambda (xs floor sym)
		   (cond
		    ((> floor 4) #f)
		    ((null? xs) #f)
		    (#t
		     (let ((sym2 (car xs)))
		       (cond
			((= floor 1) ; only up
			 (up-2 sym sym2 floor))
			((= floor 4) ; only down
			 (down-2 sym sym2 floor))
			((or (= floor 2)(= floor 3)) ;; up or down
			 (up-2 sym sym2 floor)
			 (down-2 sym sym2 floor))
			(#t (error "gen")))
		       (foo4 (cdr xs) floor sym))))))
	   )	     
    ;; all one piece moves
    (foo2 syms 1)
    ;; all two piece moves
    (foo3 syms 1)
    (set! res (cons 'begin res))
    res
    )))





(define-syntax gen-macro
  (er-macro-transformer
   (lambda (form rename compare?)
     (gen))))

;;(pp (expand* '(gen-macro)))

  ;;           e  pg pm sg sm cg cm rg rm tg tm 

(define (show-floors e pg pm sg sm cg cm rg rm tg tm s)
  (let ((floor 4))
    (let ((e  (if (= e floor) " E  " " . "))
	  (pg (if (= pg floor) " PG  " " . "))
	  (pm (if (= pm floor) " PM  " " . "))
	  (sg (if (= sg floor) " SG  " " . "))
	  (sm (if (= sm floor) " SM  " " . "))
	  (cg (if (= cg floor) " CG  " " . "))
	  (cm (if (= cm floor) " CM  " " . "))
	  (rg (if (= rg floor) " RG  " " . "))
	  (rm (if (= rm floor) " RM  " " . "))
	  (tg (if (= tg floor) " TG  " " . "))
	  (tm (if (= tm floor) " TM  " " . ")))
	  (format #t "~a ~a ~a  ~a ~a ~a ~a ~a ~a ~a ~a ~%"
		  e  pg pm  sg sm cg cm rg rm tg tm )))
  
  (let ((floor 3))
    (let ((e (if (= e floor) " E  " " . "))
	  (pg (if (= pg floor) " PG  " " . "))
	  (pm (if (= pm floor) " PM  " " . "))
	  (sg (if (= sg floor) " SG  " " . "))
	  (sm (if (= sm floor) " SM  " " . "))
	  (cg (if (= cg floor) " CG  " " . "))
	  (cm (if (= cm floor) " CM  " " . "))
	  (rg (if (= rg floor) " RG  " " . "))
	  (rm (if (= rm floor) " RM  " " . "))
	  (tg (if (= tg floor) " TG  " " . "))
	  (tm (if (= tm floor) " TM  " " . ")))
	  (format #t "~a ~a ~a  ~a ~a ~a ~a ~a ~a ~a ~a ~%"
		       e  pg pm  sg sm cg cm rg rm tg tm )))
  (let ((floor 2))
    (let ((e (if (= e floor) " E  " " . "))
	  (pg (if (= pg floor) " PG  " " . "))
	  (pm (if (= pm floor) " PM  " " . "))
	  (sg (if (= sg floor) " SG  " " . "))
	  (sm (if (= sm floor) " SM  " " . "))
	  (cg (if (= cg floor) " CG  " " . "))
	  (cm (if (= cm floor) " CM  " " . "))
	  (rg (if (= rg floor) " RG  " " . "))
	  (rm (if (= rm floor) " RM  " " . "))
	  (tg (if (= tg floor) " TG  " " . "))
	  (tm (if (= tm floor) " TM  " " . ")))
	  (format #t "~a ~a ~a  ~a ~a ~a ~a ~a ~a ~a ~a ~%"
		      e  pg pm  sg sm cg cm rg rm tg tm )))
  (let ((floor 1))
    (let ((e (if (= e floor) " E  " " . "))
	  (pg (if (= pg floor) " PG  " " . "))
	  (pm (if (= pm floor) " PM  " " . "))
	  (sg (if (= sg floor) " SG  " " . "))
	  (sm (if (= sm floor) " SM  " " . "))
	  (cg (if (= cg floor) " CG  " " . "))
	  (cm (if (= cm floor) " CM  " " . "))
	  (rg (if (= rg floor) " RG  " " . "))
	  (rm (if (= rm floor) " RM  " " . "))
	  (tg (if (= tg floor) " TG  " " . "))
	  (tm (if (= tm floor) " TM  " " . ")))
	  (format #t "~a ~a ~a  ~a ~a ~a ~a ~a ~a ~a ~a ~%"
		  e  pg pm  sg sm cg cm rg rm tg tm )))
  (when (not (null? s))
    (format #t "~a ~%~% " (car s))))

  	       



;; e elevator
;; pg pm , sg sm , cg cm , rg rm , tg tm  = generator and microchip pairs 
;; s steps to get here
(define (foo e pg pm sg sm cg cm rg rm tg tm s s2)
  ;; any-fried ?
  (let ((fried (any-fried? e pg pm sg sm cg cm rg rm tg tm))
	(is-completed (completed? e pg pm sg sm cg cm rg rm tg tm)))
    (cond
     (fried fried)
     (is-completed (format #t "solution found : ~%" )
		(pp s)
		(format #t "~%"))
     (#t
      
      ;;(show-floors e pg pm sg sm cg cm rg rm tg tm s)
      
      (gen-macro)
        ))))


(define (run)
  (let ((e 1)(pg 1)(pm 1)(sg 1)(sm 1)
	(cg 2)(cm 2)(rg 2)(rm 2)(tg 2)
	(tm 3)
	(path '())
	(history '()))
    (foo e pg pm sg sm
	 cg cm rg rm tg
	 tm
	 path
	 history)))


(run)







