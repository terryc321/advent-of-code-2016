
(import (chicken format))
(import (chicken pretty-print))
(import srfi-1)
(import simple-loops)

#|

large vector with these

0 - empty
node v next
node v next
node v next
nth node

(make-vector N + 1)

advantage now next can be an integer
value v
nodes are destructive

|#

(define-record node index value previ nexti)
(define-record-type node
  (make-node index value previ nexti)
  node?
  (index node-index node-index-set!)
  (value node-value node-value-set!)
  (previ node-previ node-previ-set!)
  (nexti node-nexti node-nexti-set!)
)

(define make-circ
  (lambda (n)
    (let ((res #f))
      (cond
       ((< n 1) (error "circ needs atleast 1 node"))
       (#t
	(set! res (make-vector (+ n 1)))
	(do-for (i 1 (+ n 1))
		(vector-set! res i (make-node i 1 (- i 1) (+ i 1)))
		(when (= i 1)
		  (vector-set! res i (make-node i 1 n 2 )))
		(when (= i n)
		  (vector-set! res i (make-node i 1 (- n 1) 1 ))))
	res)))))

(define-record-printer (node node out)
  (fprintf out "#,(node [~s]  value(~s) previ(~s) nexti(~s))"
	   (node-index node) (node-value node) (node-previ node) (node-nexti node)))


;; (define input (lambda () (circ 3005290)))

;; steal ? 
;; (define steal
;;   (lambda (i)
;;     (
#|

 5 <- 1 -> 2 -> 3 -> 4 -> 5 -> ... 1
           *
 remove 2 say ,            

|#

(define prev
  (lambda (v i)
    (let ((node (vector-ref v i)))
      (node-previ node))))

(define next
  (lambda (v i)
    (let ((node (vector-ref v i)))
      (node-nexti node))))
    

(define remove-node
  (lambda (v i)
    (let ((len (vector-length v)))
      (cond
       ((or (<= i 0) (> i (- len 1))) (error "remove-node index out bounds"))
       (#t (let* ((node (vector-ref v i))
		  (nexti (prev v i))
		  (previ (next v i)))
	     (format #t "removing node ~a : previ ~a : nexti ~a ~%" i previ nexti)
	     (let* ((pnode (vector-ref v previ))
		    (val2 (node-value pnode))
		    (prev (node-previ pnode))
		    (next (node-nexti pnode)))
	       (vector-set! v previ (make-node previ val2 prev nexti)))
	     (let* ((pnode (vector-ref v nexti))
		    (val3 (node-val pnode))
		    (prev (node-previ pnode))
		    (next (node-nexti pnode)))
	       (vector-set! v nexti (make-node nexti val3 previ next)))
	     (vector-set! v i #f)
	     ))))))




(define demo
  (lambda ()
    (let ((clist (make-circ 5)))
      (play clist))))


(define play
  (lambda (v)
    (letrec ((foo (lambda (i)
		    (format #t "debugging elf loop .....~%")
		    (pp v)

		    (let* ((node (vector-ref v i))
			   (nexti (node-nexti node)))
		      (cond
		       ((= nexti i)
			(format #t "no more nodes except survivor node~%")
			(pp node)
			node)
		       (#t
			(format #t "eliminating elf ~a ~%" nexti)
			(let* ((next-node (vector-ref v nexti))
			       (value (node-value next-node))
			       (next-next (node-nexti next-node))
			       (new-value (+ 1 (node-value node))))			      
			  (remove-node v nexti)
			  (vector-set! v i (make-node i
						      new-value
						      (node-previ node)
						      next-next))
			  (format #t "setting node ~a to value ~a ~%" i new-value)
			  (foo next-next))))))))
      (foo 1))))






	
    





      

	     
		    
	     











