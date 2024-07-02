
;; guile does not support ~ home directory in pathname ??
;; (chdir "/home/terry/code/advent-of-code/advent-of-code-2016/day21/guile")
;; (getcwd)
;; (+ 1 2)

(import (ice-9 format))
(import (srfi srfi-1))
(import (ice-9 pretty-print))
(import (ice-9 match))

(define pp pretty-print)


(define word (string-copy "012345678"))

(define (swapxy x y)
  (let ((a (string-ref word x))
	(b (string-ref word y)))
    ;; (format #t "swapping x ~a (~a): y ~a (~a) ~%" x a y b)
    (string-set! word x b)
    (string-set! word y a)
    ))


(define (find-index c)
  (let ((len (string-length word)))
    (letrec ((foo (lambda (i)
		    (cond
		     ((>= i len) -1)
		     (#t (let ((c2 (string-ref word i)))
			   (cond
			    ((char=? c c2) i)
			    (#t (foo (+ i 1))))))))))
      (foo 0))))


#|
figure out the indices for this ,

              word
            0 1 2 3 4 5 6 7 8 
                  5 4 3
swap reverse 3 .. 5 in string 
               5 -> 3
               4 -> 4
      (y-n)    3 -> 5  (x + n)
|#
(define (rev x y)
  ;; (format #t "REVERSING LETTERS ~a thru ~a ~%" x y)
  (let* ((lim (- y x))
	 (copy (string-copy word)))
    (letrec ((foo (lambda (word n)
		    (cond
		     ((> n lim)
		      ;; (format #t "word AFTER => ~a ~%" word)
		      word)
		     (#t
		      (let ((a (+ x n))
			    (b (- y n)))
			(let ((ch1 (string-ref copy a))
			      (ch2 (string-ref copy b)))
			  ;; (format #t "swapping indices ~a <-> ~a  : ~a ~a ~%" a b ch1 ch2)
			  (string-set! word a ch2)
			  ;; (format #t "word I => ~a ~%" word)
			  (string-set! word b ch1)
			  ;; (format #t "word II => ~a ~%" word)
			  (foo word (+ n 1)))))))))
      (foo word 0)
      ;; (format #t "word CONCLUSION => ~a ~%" word)      
      )))



#|
rotate word left [n] 
              word
          0 1 2 3 4 5 6 7 8
          1 2 3 4 5 6 7 8 0 rot 1              
          2 3 4 5 6 7 8 9 0 rot 2
          3 4 5 6 7 8 9 0 1 rot 3
          4 5 6 7 8 9 0 1 2 rot 4
count up from n until hit len of string then reset counter
keep counting up
|#
(define (rotleft x)
  ;;(format #t "ROT LEFT ~a ~% " x)
  (let* ((lim (string-length word))
	 (copy (string-copy word)))
    (letrec ((foo (lambda (n c)
		    (cond
		     ((>= n lim) word)
		     ((>= c lim) (foo n 0))
		     (#t
		      (let ((ch1 (string-ref copy c)))
			  (string-set! word n ch1)
			  (foo (+ n 1) (+ c 1))))))))
      (foo 0 (modulo x lim)))))

#|
rotate word right [n] 
              word
          0 1 2 3 4 5 6 7 8
          8 0 1 2 3 4 5 6 7 rot 1
          7 8 0 1 2 3 4 5 6 rot 2
          6 7 8 9 0 1 2 3 4 rot 3
          5 6 7 8 9 0 1 2 3 rot 4

|#
(define (rotright x)
  ;; (format #t "ROT RIGHT ~a ~%" x)
  (let* ((lim (string-length word))
	 (copy (string-copy word)))
    (letrec ((foo (lambda (n c)
		    (cond
		     ((>= n lim) word)
		     ((>= c lim) (foo n 0))
		     (#t
		      ;; (format #t "RTR word => ~a : n=~a c=~a ~%" word n c)		    
		      (let ((ch1 (string-ref copy n)))
			  (string-set! word c ch1)
			  (foo (+ n 1) (+ c 1))))))))
      (foo 0 (modulo x lim)))))


    

(define (swapletter a b)
  ;; find letter a index
  ;; find letter b index
  ;; swapxy x y
  (let ((ia (find-index a))
	(ib (find-index b)))
    ;;(format #t "ia = ~A : ib = ~a ~%" ia ib)
    (swapxy ia ib)
    word))



#|
              SRC     DEST
           move x - > y
  x < y            
                x      y
          0 1 2 3  4 5 6 <3> 7  8
          0 1 2 <> 4 5 6 <3> 7 8
     sub 0 -> x = 0 1 2
     sub x+1 -> y  = 4 5 
     sub x -> x+1 = "3"
     sub y -> len = 6 7 8
|#
(define (move-x-less-y word x yy)
  (let* ((lim (string-length word))
	 (copy (string-copy word)))
    (let ((y (+ yy 1)))
      (string-append (substring word 0 x)
		     (substring word (+ x 1) y)
		     (substring word x (+ x 1))
		     (substring word y lim)))))
#|
              SRC     DEST
           move x - > y
  x > y           
                 y       x
          0 1 2 <6> 4 5  6  7 8
          0 1 2 <6> 4 5 < > 7 8
     sub 0 -> y = 0 1 2
     sub x -> x+1 = "6"
     sub y+1 -> x  = 4 5 
     sub x -> len = 6 7 8
|#
(define (move-x-more-y word x y)
  (let* ((lim (string-length word))
	 (copy (string-copy word)))
    (string-append (substring word 0 y)
		   (substring word x (+ x 1))
		   (substring word y x)
		   (substring word (+ x 1) lim))))


(define (move-helper word x y)
  (cond
   ((< x y) (move-x-less-y word x y))
   ((= x y) word)
   ((> x y) (move-x-more-y word x y))))

(define (move x y)
  ;; (format #t "MOVE ~a ~a ~%" x y)
  (set! word (move-helper word x y ))
  word)


(define (rotpos c)
  ;; (format #t "ROTPOS ~a : " c)
  (let ((i (find-index c)))
    ;; (format #t "index ~a ~%" i)
    (rotright (+ i 1))
    (when (>= i 4) (rotright 1))))


(define (insert-into xs)
  (cons (car xs) (cons 'word (cdr xs))))


(define (run prog target)
  (set! word target)
  (let ((ys (map (lambda (x) `(begin
				,x
				;;(format #t "word => ~a ~%" word)
				))
		 prog)))
    (eval 
     `(begin
	;;(define word (string-copy target))
	,@ys)
     (interaction-environment))))



(define (demo)
  (let ((prog   `((swapxy 4 0)
		  (swapletter #\d #\b)
		  (rev 0 4)
		  (rotleft 1)
		  (move 1 4)
		  (move 3 0)
		  (rotpos #\b)
		  (rotpos #\d)))
	(target (string-copy "abcde")))
    (run prog target)))


(define (puzzle in)
  (let ((prog  '((rotpos #\d)
		 (move 1 6)
		 (swapxy 3 6)
		 (rotpos #\c)
		 (swapxy 0 1)
		 (rotright 5)
		 (rotleft 3)
		 (rotpos #\b)
		 (swapxy 0 2)
		 (rotpos #\g)
		 (rotleft 0)
		 (rev 0 3)
		 (rotpos #\a)
		 (rotpos #\h)
		 (rotpos #\a)
		 (rotpos #\g)
		 (rotleft 5)
		 (move 3 7)
		 (rotright 5)
		 (rotpos #\f)
		 (rotright 7)
		 (rotpos #\a)
		 (rotright 6)
		 (rotpos #\a)
		 (swapletter #\c #\f)
		 (rev 2 6)
		 (rotleft 1)
		 (rev 3 5)
		 (rotpos #\f)
		 (swapxy 6 5)
		 (swapletter #\h #\e)
		 (move 1 3)
		 (swapletter #\c #\h)
		 (rev 4 7)
		 (swapletter #\f #\h)
		 (rotpos #\f)
		 (rotpos #\g)
		 (rev 3 4)
		 (rotleft 7)
		 (swapletter #\h #\a)
		 (rotpos #\e)
		 (rotpos #\f)
		 (rotpos #\g)
		 (move 5 0)
		 (rotpos #\c)
		 (rev 3 6)
		 (rotright 4)
		 (move 1 2)
		 (rev 3 6)
		 (swapletter #\g #\a)
		 (rotpos #\d)
		 (rotpos #\a)
		 (swapxy 0 7)
		 (rotleft 7)
		 (rotright 2)
		 (rotright 6)
		 (rotpos #\b)
		 (rotright 2)
		 (swapxy 7 4)
		 (rotleft 4)
		 (rotleft 3)
		 (swapxy 2 7)
		 (move 5 4)
		 (rotright 3)
		 (rotpos #\g)
		 (move 1 2)
		 (swapxy 7 0)
		 (move 4 6)
		 (move 3 0)
		 (rotpos #\f)
		 (swapletter #\g #\d)
		 (swapxy 1 5)
		 (rev 0 2)
		 (swapxy 7 3)
		 (rotpos #\g)
		 (swapletter #\c #\a)
		 (rotpos #\g)
		 (rev 3 5)
		 (move 6 3)
		 (swapletter #\b #\e)
		 (rev 5 6)
		 (move 6 7)
		 (swapletter #\a #\e)
		 (swapxy 6 2)
		 (move 4 5)
		 (rotleft 5)
		 (swapletter #\a #\d)
		 (swapletter #\e #\g)
		 (swapxy 3 7)
		 (rev 0 5)
		 (swapxy 5 7)
		 (swapxy 1 7)
		 (swapxy 1 7)
		 (rotright 7)
		 (swapletter #\f #\a)
		 (rev 0 7)
		 (rotpos #\d)
		 (rev 2 4)
		 (swapxy 7 1)
		 (swapletter #\a #\h)
		 )))
    (run prog in)))

(define rec
  (lambda (letters choice)
    (cond
     ((null? letters)
      (let* ((in (list->string choice))
	     (in2 (string-copy in))
	     (out "fbgdceah")
	     (out2 (string-copy out)))
	(format #t "trying ~A ~%" in)
	(set! word in)
	(puzzle in)
	(when (equal? word out2)
	  (format #t "solution in  ~a <-> out  ~a ~%" in2 out2)
	  )))
     (#t
      (letrec ((foo (lambda (xs)
		      (cond
		       ((null? xs) #f)
		       (#t (let ((ch (car xs)))
			     (rec (filter (lambda (x)(not (char=? x ch))) letters)
				  (cons ch choice))
			     (foo (cdr xs))))))))
	(foo letters))))))




(define brute
  (lambda ()
    (let ((letters '(#\a #\b #\c #\d #\e #\f #\g #\h)))
      (rec letters '()))))

#|

solution gcdaebfh <-> fbgdceah 
........ rejected .......


(puzzle (string-copy "fhgcdaeb"))
= "fbgdceah"
.......... ACCEPTED ......>>>... fhgcdaeb ...<<<.... 

difficult puzzle in scheme because strings "alphabetic" is treated as a constant immutable string
what probably be better would be some sort of vector implementation like a grid
        word at top
        .....
        .....
        .... 
        ....
        .... 
        each stage of processing

   idea that if all an algorithm does is shuffle the same way each time 
   1 2 3 4
   4 3 2 1

   then there is a simple mapping 1 : 1 
   if known mapping then the inverse is simply looking at where original sequence went 

no because rotations are dependent on where letters are in the sequence ,
 so no 1 : 1 mapping possible


|#



     
