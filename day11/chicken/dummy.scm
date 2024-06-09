

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

