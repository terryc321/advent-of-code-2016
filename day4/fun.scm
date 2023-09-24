
(import scheme)
(import (chicken format))
(import (chicken sort))

(import srfi-1)

(define input
  (with-input-from-file "input"
    (lambda ()
      (read))))


(define sector-id-sum 0)


(define (read-values xs)  
  (cond
   ((null? xs) #f)
   (#t (let ((a (first xs)))
	 (process a)
	 (read-values (drop xs 1))))))



#|
a - z
dash -
0-9 +  .... sector ID 
[  ......... checksum 
a - z 
]
|#
(define (process str)
  ;;(format #t "processing ~a ~%" str)
  (let ((i 0)
	(len (string-length str))
	(ni #f)
	(nk #f)
	(ci #f)
	(ck #f))
    (letrec ((foo (lambda (i)
		    (cond
		     ((>= i len) #f)
		     (#t
		      (cond
		       ((and (not ni) (char-numeric? (string-ref str i)))
			(set! ni i))
		       ((char-numeric? (string-ref str i))
			(set! nk i))
		       (#t #f))
		      (foo (+ i 1)))))))
      (foo 0)
      (let ((word (word-pop (substring str 0 ni)))
	    (id (string->number (substring str ni (+ nk 1))))
	    (check (substring str (+ nk 2) (- len 1))))
	(when (string=? word check)
	  (format #t "proper ~a ~%" word)
	  (set! sector-id-sum (+ sector-id-sum id))
	  )
	;; (format #t "word ~a to ~a : [~a] " 0 (- ni 1) word)
	;; (format #t "id ~a to ~a : [~a] " ni nk id)
	;; (format #t "check ~a to ~a : *~a* " (+ nk 2) (- len 1) check)
	;; (format #t "~%")
	))))


(define alphabet (map integer->char (map (lambda (x) (+ x 97)) (iota 26))))


;; char->integer #\a
(define (word-pop str)
  (let ((arr (make-vector 26 0))
	(i 0)
	(len (string-length str)))
    (letrec ((foo (lambda (i)
		    (cond
		     ((>= i len) #f)
		     (#t
		      (let ((ch (string-ref str i)))
			(cond
			 ((char=? ch #\-) #f)
			 (#t (let ((n (- (char->integer ch) (char->integer #\a))))
			       (vector-set! arr n (+ 1 (vector-ref arr n)))))))
		      (foo (+ i 1)))))))
      (foo 0)
      (let ((xs '()))
	(letrec ((loop (lambda (i)
			 (cond
			  ((>= i 26) #f)
			  (#t
			   (cond
			    ((> (vector-ref arr i) 0)
			     (set! xs (cons (list (list-ref alphabet i) (vector-ref arr i))
					    xs)))
			    (#t #f))
			   (loop (+ i 1)))))))
	  (loop 0))
	(my-join (ties (sort xs (lambda (x y) (> (second x)(second y))))))))))

;; xs is sorted
(define (ties xs)
  (cond
   ((null? xs) '())
   (#t (let* ((hd (car xs))
	      (n (second hd))
	      (got (sort (filter (lambda (x) (= (second x) n)) xs)
			 (lambda (x y) (char<? (first x) (first y))))))
	 (append got (ties (drop xs (length got))))))))


(define (my-join xs)
  (let ((str ""))
    (letrec ((loop (lambda (ys)
		     (cond
		      ((null? ys) #f)
		      (#t (set! str (string-append str (format #f "~a" (first (first ys)))))
			  (loop (cdr ys)))))))
      (loop xs)
      (substring str 0 5))))



	 
#|

;; get 2nd element and enter into xs if not already in xs
(define (ties2 xs)
  (let ((res '()))
    (letrec ((loop (lambda (ys)
		     (cond
		      ((null? ys) #t)
		      (#t
		       (cond
			((member (car ys) res) #f)
			(#t (set! res (cons (car ys) res))))
		       (loop (cdr ys)))))))
      (loop xs)
      res)))

;; values 
(define (ties4 xs)
  (sort (ties2 (map second xs)) >))

;; ties5 get all those with n in second element
(define (ties5 xs n)
  (filter (lambda (x) (= (second x) n))
	  xs))
;;
(define (ties xs)
  (ties3 xs (second (car xs))))

(define (ties3 xs n)
  (let ((res '()))
    (letrec ((loop (lambda (ys)
		     (cond
		      ((null? ys) #t)
		      (#t
		       (cond
			((member (car ys) res) #f)
			(#t (set! res (cons (car ys) res))))
		       (loop (cdr ys)))))))
      (loop xs)
      res)))

|# 


(define (sector-id str)
  #t)

(define (checksum str)
  #t)


(define (test)
  (process "aaaaa-bbb-z-y-x-123[abxyz]")
  (process "a-b-c-d-e-f-g-h-987[abcde]")
  (process "not-a-real-room-404[oarel]")
  (process "totally-real-room-200[decoy]"))



(define (solve)
  (set! sector-id-sum 0)
  (read-values input)
  sector-id-sum
  )



#|
...
proper bwhoc 
137896

sector id sum 137896


|#
