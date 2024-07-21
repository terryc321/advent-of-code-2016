
(import (chicken format))
(import (chicken pretty-print))
(import srfi-1)
(import simple-loops)
(import bindings)

#|
each entry in array needs to be either
#f it has been erased
vector of 2 elements value next

simpler version where just a straight vector of values
when remove an element simply duplicate vector with that value removed ,
shrink vector

since going to be completely



|#

(define (make-circ n)
  (let ((res (make-vector (+ n 1))))
    (do-for (i 1 (+ n 1))
	    (vector-set! res i (list i 1)))
    (vector-set! res 0 #f)
    res))
	

#|
           1
         5   2
          4 3
odd
|#
(define (opposite n)  (floor (/ n 2)))


;; (define copy-down
;;   (lambda (v k)
;;     (let* ((lim (vector-length v))
;; 	   (val-a (vector-ref v 0))
;; 	   (res (make-vector (- lim 1))))
;;       ;; lim-1 last element
;;       ;; lim-2 : do-for go upto inclusive lim-2
;;       (let ((r 1))
;; 	(vector-set! res 0 #f)	
;; 	(do-for (i 1 lim)
;; 		(cond
;; 		 ((= i k) #f)
;; 		 (#t
;; 		  ;; (format #t "copying i (~a) -> r (~a) ~%" i r)
;; 		  (vector-set! res r (vector-ref v i))
;; 		  (set! r (+ r 1))))))
;;       (let ((a (vector-ref res 1)))
;; 	(do-for (i 2 (- lim 1))
;; 		(vector-set! res (- i 1) (vector-ref v i)))
;; 	(vector-set! res (- lim 2) a)
;; 	res))))


(define find-next
  (lambda (v k j)
    ;;(format #t "find next v = ~a , k = ~a , j = ~a ~%" v k j)
    (let ((lim (vector-length v)))
      (letrec ((foo (lambda (i)
		      (cond
		       ((>= i lim) (foo 1))
		       ((< i lim)
			(let ((a (vector-ref v i)))
			  (cond
			   (a (cond
			       ((= j 1) i)
			       (#t (set! j (- j 1))
				   (foo (+ i 1)))))
			   (#t (foo (+ i 1))))))))))
	(foo (+ k 1))))))





(define demo
  (lambda (n)
    (let ((v (make-circ n))
	  (play n))
      (letrec ((foo (lambda (i) 
		      ;;(pp v)
		      (when (zero? (modulo play 1000))
			(format #t "number of players ~a ~%" play))
		      (let* ((op (opposite play))
			     (len (vector-length v)))
			(cond
			 ((<= len 2) 'done)			 
			 (#t
			  (let ((iv (vector-ref v i))
				(j (find-next v i op)))
			    ;;(format #t "opposite index is ~a ~%" j)
			    (cond
			     ((= i j) (error "i = j "))
			     (#t
			      (let ((iv2 (vector-ref v j)))				   
			    (bind (index val) iv
				  (bind (index2 val2) iv2
					(vector-set! v i (list index (+ val val2)))))
			    (vector-set! v j #f)
			    (set! play (- play 1))
			    (let ((nexti (find-next v i 1)))
			      ;;(format #t "next after ~a is ~a ~%" i nexti)
			      (cond
			       ((= i nexti) (vector-ref v i))
			       (#t
				(foo nexti))))))))))))))
	(foo 1)))))



(format #t "solution is ~A ~%" (demo 3005290))




			       
      









