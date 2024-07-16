
(import scheme)
(import (chicken format))
(import (chicken sort))
(import (chicken pretty-print))
(import (chicken irregex))
;;(import (srfi srfi-115))

(import bindings)


(import procedural-macros)

;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))

(import sequences)

(import srfi-1)


;;(define port (open-input-file "input"))
;; parse "(3x4)"
;;(define (n-by-m s) #t)

; input as a string
(define input 
  (with-input-from-file "input"
    (lambda ()
      (read))))

;; what if do not have regexpr available ?
;; (let ((match (irregex-search (irregex "^[(]([0-9]+)x([0-9]+)[)]" 'i) "(123x456)FOOBARabcFOOBARdef")))
;;   match)

;;(string-match (regexp "^[(]([0-9]+)x([0-9]+)[)]") input)
;; (let ((re (regexp-escape "'('[(]([0-9]+)")))
;;   (string-match re input))
;; 
;; (let ((re (regexp-escape "'('")))
;;   (string-match re input))

;;(regexp-escape "^[0-9]+:.*$")

;; if char = ( then next + parse-int
;; 	    if char = x then next + parse-int
;;  	    if char = ) then next
;;                 apply transformation theoretical ?
                   

;; return pair #t and int value and offset
;; if no int then
;; as a tuple (#t , VAL , OFFSET)
;; otherwise  (#f , 0   , 0)  dummy values ...
;; expected integer lead with non-zero value
;; cc = char count

;; return some absurd character not match integer or letter
(define safe-string-ref
  (lambda (s i)
    (let ((len (string-length s)))
      (cond
       ((and (>= i 0) (< i len)) (string-ref s i))
       (#t #\!)))))
     

(define parse-int
  (lambda (s i)
    (let ((lim (string-length s)))
      (letrec ((foo (lambda (i cc val)
		      (cond
		       ((< i lim)
			(let ((ch (safe-string-ref s i)))
			  ;; (format #t "looking at ~a ~%" ch)
			  (cond
			   ((and (not val)
				 (or
				  (char=? ch #\0)
				  (char=? ch #\1)(char=? ch #\2) (char=? ch #\3)
				  (char=? ch #\4) (char=? ch #\5)(char=? ch #\6)
				  (char=? ch #\7) (char=? ch #\8)(char=? ch #\9)))
			    ;; (format #t "at L72 ~a ~%" ch)			    
			    (let ((val2 (- (char->integer ch) (char->integer #\0)))
				  (cc2 1))
			      (foo (+ i 1) cc2 val2)))
			   ((and val
				 (or
				  (char=? ch #\0)
				  (char=? ch #\1)(char=? ch #\2) (char=? ch #\3)
				  (char=? ch #\4) (char=? ch #\5)(char=? ch #\6)
				  (char=? ch #\7) (char=? ch #\8)(char=? ch #\9)))
			    ;; (format #t "at L81 ~a ~%" ch)			    
			    (let ((val2 (+ (* 10 val) (- (char->integer ch) (char->integer #\0)))))
			      (foo (+ i 1) (+ cc 1) val2)))
			   (val
			    ;; (format #t "at L85 ~a ~%" ch)		
			    (list #t val i))
			   (#t
			    ;; (format #t "at L88 ~a ~%" ch)		
			    (list #f 0 0)))))
		       (val
			;; (format #t "at L92 ~%" )			    
			(list #t val i))
		       (#t
			;; (format #t "at L96 ~%")			    
			(list #f 0 0))))))
	(foo i 0 #f)))))


;; bloated by error correcting code and mindless endless checks
;; lack of debugger etc...
;; (define parse-pair
;;   (lambda (s i)
;;     (let ((lim (string-length s)))
;;       (cond
;;        ((< i lim)	
;; 	(let ((ch (safe-string-ref s i)))
;; 	  (format #t "1)look at char [~a] at index ~a ~%" ch i)
;; 	  (cond
;; 	   ((char=? ch #\( )
;; 	    (let ((k (+ i 1)))
;; 	      (format #t "1b) parse int at index ~a ~%" k)
;; 	      (let ((alpha (parse-int s k)))
;; 		(bind (is1 val1 cc1) alpha
;; 		      (format #t "found first value [~a]~%" val1)
;; 		      (let ((ch (safe-string-ref s (+ cc1 1))))
;; 			(format #t "character after k cc1 is [~a]~%" ch)
;; 			(cond
;; 			 ((char=? #\x ch) 
;; 			  (let* ((k2 (+ cc1))
;; 				 (k2b (+ k2 1))
;; 				 (ch2 (safe-string-ref s k2)))			
;; 			    (format #t "2)look at char [~a] at index ~a ~%" ch2 k2)
;; 			    (let ((beta (parse-int s k2b)))
;; 			      (bind (is2 val2 cc2) beta
;; 				    (format #t "found second value [~a]~%" val2)
;; 				    (let* ((k3 (+ k2b cc2))
;; 					   (ch3 (safe-string-ref s k3)))
;; 				      (format #t "3)look at char [~a] at index ~a ~%" ch3 k3)
;; 				      (cond
;; 				       ((char=? ch3 #\))
;; 					(format #t "4)found ~a ~a ~%" val1 val2)
;; 					#t))))))))))))))))))))

    ;; (let ((lim (string-length s)))
    ;;   (cond
    ;;    ((< i lim)	
    ;; 	(let ((ch (safe-string-ref s i)))
    ;; 	  (format #t "1)look at char [~a] at index ~a ~%" ch i)
    ;; 	  (cond
    ;; 	   ((char=? ch #\( )
    ;; 	    (let ((k (+ i 1)))
    ;; 	      (format #t "1b) parse int at index ~a ~%" k)
    ;; 	      (let ((alpha (parse-int s k)))
    ;; 		(bind (is1 val1 cc1) alpha
    ;; 		      (format #t "found first value [~a]~%" val1)
    ;; 		      (let ((ch (safe-string-ref s (+ cc1 1))))
    ;; 			(format #t "character after k cc1 is [~a]~%" ch)
    ;; 			(cond
    ;; 			 ((char=? #\x ch) 
    ;; 			  (let* ((k2 (+ cc1))
    ;; 				 (k2b (+ k2 1))
    ;; 				 (ch2 (safe-string-ref s k2)))			
    ;; 			    (format #t "2)look at char [~a] at index ~a ~%" ch2 k2)
    ;; 			    (let ((beta (parse-int s k2b)))
    ;; 			      (bind (is2 val2 cc2) beta
    ;; 				    (format #t "found second value [~a]~%" val2)
    ;; 				    (let* ((k3 (+ k2b cc2))
    ;; 					   (ch3 (safe-string-ref s k3)))
    ;; 				      (format #t "3)look at char [~a] at index ~a ~%" ch3 k3)
    ;; 				      (cond
    ;; 				       ((char=? ch3 #\))
    ;; 					(format #t "4)found ~a ~a ~%" val1 val2)
    ;; 					#t))))))))))))))))))))



(define parse-pair
  (lambda (s i)
    ;; implicit i
    (call/cc
     (lambda (exit)
       (letrec ((val1 0)
		(val2 0)
		(is (lambda (ch) (char=? (safe-string-ref s i) ch)))
		(is-digit? (lambda ()
			     (let ((ch (safe-string-ref s i)))
			       (or (char>=? ch #\0)
				   (char<=? ch #\9)))))
		(next (lambda () (set! i (+ i 1))))
		(parse (lambda () ;; open parens
			 (if (is #\( )
			     (begin (next) (parse2))
			     (exit #f))))
		(parse2 (lambda () ;; an integer
			  (if (is-digit?)
			      (begin (bind (is val cc) (parse-int s i)
					   (when (not is) (exit #f))
					   (set! val1 val)
					   ;;(format #t "first val ~a ~%" val1)
					   (set! i cc)
					   (parse3)
					   ))
			      (exit #f))))
		(parse3 (lambda () ;; a letter x			  
			 (if (is #\x )
			     (begin (next) (parse4))
			     (exit #f))))
		(parse4 (lambda () ;; an integer
			  (if (is-digit?)
			      (begin (bind (is val cc) (parse-int s i)
					   (when (not is) (exit #f))
					   (set! val2 val)
					   ;;(format #t "second val ~a ~%" val2)
					   (set! i cc)
					   (parse5)
					   ))
			      (exit #f))))
		(parse5 (lambda () ;; close parens
			 (if (is #\) )
			     (begin (next) (exit (list val1 val2 i)))
			     (exit #f)))))		
	 (parse))))))



(parse-pair "0" 0)
(parse-pair "(123x456)asdf" 0)
(parse-pair "a(123x456)asdf" 1)
(parse-pair "aa(123x456)asdf" 2)
(parse-pair "aaa(123x456)asdf" 3)
			    
;; parse (XXXxYYYY) what does XXX and YYY mean ?
;; take X characters follow marker and repeat them Y times


    ;; ADVENT contains no markers and decompresses to itself with no changes, resulting in a decompressed length of 6.
    ;; A(1x5)BC repeats only the B a total of 5 times, becoming ABBBBBC for a decompressed length of 7.
    ;; (3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
    ;; A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG for a decompressed length of 11.
    ;; (6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, but because it's within a data section of another marker, it is not treated any differently from the A that comes after it. It has a decompressed length of 6.
    ;; X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of 18), because the decompressed data from the (8x2) marker (the (3x3)ABC) is skipped and not processed further.
;;(recur "ADVENT") ==> 6
;;(recur "A(1x5)BC") ==> 7 
;; (recur "(3x3)XYZ")  9
;; (recur "A(2x2)BCD(2x2)EFG") 11
;; (recur "(6x1)(1x3)A")  6
;; (recur "X(8x2)(3x3)ABCY")  18

(define recur
  (lambda (s)
    (let ((i 0)
	  (lim (string-length s))
	  (len 0))
      (letrec ((foo (lambda ()
		      (cond
		       ((< i lim)
			(let ((vals (parse-pair s i)))
			  (cond
			   (vals (bind (x y cc) vals		       
				       (format #t "~a ~a ~a ~%" x y cc)
				       (let ((sub (substring s cc (+ cc x))))
					 (set! len (+ len (* x y)))
					 (format #t "substring ~a ~%" sub)
					 (set! i (+ cc x))
					 (foo)
					 )))			 
			   (#t (set! len (+ len 1)) ;; normal character
			       (set! i (+ i 1))
			       (foo)))))))))
	(foo)
	len))))

;; (recur input) ==> 115118  ..... ACCEPTED



;; --------- test cases ----------- 

    ;; (3x3)XYZ still becomes XYZXYZXYZ, as the decompressed section contains no markers.
    ;; X(8x2)(3x3)ABCY becomes XABCABCABCABCABCABCY, because the decompressed data from the (8x2) marker is then further decompressed, thus triggering the (3x3) marker twice for a total of six ABC sequences.
    ;; (27x12)(20x12)(13x14)(7x10)(1x12)A decompresses into a string of A repeated 241920 times.
    ;; (25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN becomes 445 characters long.
;; (recur2 "(3x3)XYZ") 9
;; (recur2 "(27x12)(20x12)(13x14)(7x10)(1x12)A")  241920
;; (recur2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")  445		      

(define recur2
  (lambda (s)
    (let ((i 0)
	  (lim (string-length s))
	  (len 0))
      (letrec ((foo (lambda ()
		      (cond
		       ((< i lim)
			(let ((vals (parse-pair s i)))
			  (cond
			   (vals (bind (x y cc) vals		       
				       (format #t "~a ~a ~a ~%" x y cc)
				       (let ((sub (substring s cc (+ cc x))))
					 (let ((len2 (recur2 sub)))
					   (set! len (+ len (* len2 y)))
					   (format #t "substring ~a ~%" sub)
					   (set! i (+ cc x))
					   (foo)
					 ))))			 
			   (#t (set! len (+ len 1)) ;; normal character
			       (set! i (+ i 1))
			       (foo)))))))))
	(foo)
	len))))
	     

;; (recur2 input) ==> 11107527530 ....... ACCEPTED .....




			 













      
