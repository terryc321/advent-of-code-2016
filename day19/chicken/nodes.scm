
(import (chicken format))
(import (chicken pretty-print))
(import srfi-1)
(import simple-loops)


(define make-node (lambda (value next)
		    (list->vector `(,value ,next)))) 

(define node? (lambda (node)
		(and (vector? node)
		     (= (vector-length node) 2))))

(define node-next (lambda (node) (vector-ref node 1)))
(define node-value (lambda (node) (vector-ref node 0)))

(define make-circ
  (lambda (n)
    (let ((res #f))
      (cond
       ((< n 1) (error "circ needs atleast 1 node"))
       (#t
	(set! res (make-vector (+ n 1)))
	(do-for (i 1 (+ n 1))
		(vector-set! res i (make-node 1 (+ i 1)))
		(when (= i 1)
		  (vector-set! res i (make-node 1 2)))
		(when (= i n)
		  (vector-set! res i (make-node 1 1 ))))
	res)))))
    

(define find-next
  (lambda (circ n)
    (let ((lim (vector-length circ)))
      (call/cc (lambda (exit)
		 (do-for (i (+ n 1) lim)
			 (let ((node (vector-ref circ i)))
			   (when node
			     (exit i))))
		 (do-for (i 1 (+ n 1))
			 (let ((node (vector-ref circ i)))
			   (when node
			     (exit i))))
		 (error "find-next: should not reach here"))))))


;; how many 
(define demo
  (lambda (n)
    (let ((circ (make-circ n))
	  (play n))
      (letrec ((forward (lambda (i n)
			  (cond
			   ((= n 1) i)
			   (#t
			    (forward (find-next circ i) (- n 1))))))			    
	       (foo (lambda (i)
		      (format #t "next player is ~a ~%" i)
		      (pp circ)
		      (let ((k (forward i (+ 1 (floor (/ play 2)))))
			    (v (node-value (vector-ref circ i))))
			(format #t "eliminated player is ~a ~%" k)
			(cond
			 ((= k i)
			  (list 'last i 'with-value v))
			 (#t (let ((j (node-next (vector-ref circ k)))
				   (v2 (node-value (vector-ref circ k))))
			       ;; k is node to be deleted
			       ;; v value from deleted k
			       ;; j is the next index to look at

			       ;; delete
			       (vector-set! circ k #f)
			       ;; one less player
			       (set! play (- play 1))
			       ;; update i
			       (vector-set! circ i (make-node (+ v v2) j))
			       ;;
			       (foo j))))))))
	(foo 1)))))

(define part-b
  (lambda ()
    (format #t "~a ~%" (demo 3005290))))


;;(part-b)


  


			       
      









