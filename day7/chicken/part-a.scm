

(import scheme)
(import (chicken format))
(import (chicken sort))
;;(import regex) ;; old library
(import (chicken irregex))
(import procedural-macros)
(import srfi-13) ;; string library
(import srfi-1)
(import (chicken pretty-print)) ;; pp 
(import (chicken process-context)) ;; current-directory
(import bindings) ;; bind
(import (chicken io)) ;; read-line
(import simple-loops) ;; 

(define reader
  (lambda ()
    (with-input-from-file "../input.txt"
      (lambda ()
	(letrec ((read-all-lines (lambda ()
				   (let ((lines '()))					 
				     (letrec ((foo (lambda ()
						     (let ((line (read-line)))
						       (cond
							;; no more lines
							((eof-object? line) (reverse lines))
							(#t (set! lines (cons line lines))
							    (foo)))))))
				       (foo))))))
	  (read-all-lines)	  
	  )))))


;; changed the data so [ ] replaced with < >
;; hopefully writing regular expressions is not not so hard
(define input (reader))
(set! input (map (lambda (x) (irregex-replace/all "['[']" x "<")) input))
(set! input (map (lambda (x) (irregex-replace/all "]" x ">")) input))

;; split strings into character blocks outside a-z...[ ]a-z...
;; and inside [a-z...]
(define decode-bracket
  (lambda (s k)
    (let ((len (string-length s)))
      (call/cc (lambda (exit)
		 (do-for (i k len)
			 (let ((ch (string-ref s i)))
			   (cond
			    ((char=? ch #\>)
			     (exit i))))))))))


(define get-inside
  (lambda (s)
    (let* ((len (string-length s))
	   (res '())
	   (note (lambda (s) (set! res (cons s res)))))
      (do-for (i 0 len)
	      (let ((ch (string-ref s i)))
		;;(format #t "ch = ~a ~%" ch)
		(cond
		 ((char=? ch #\<)
		  (let* ((j (decode-bracket s (+ i 1)))
			 (sub (substring s (+ i 1) j)))
		    (note sub))))))
      (reverse res))))


(define get-outside-helper
  (lambda (s k)
    (let* ((len (string-length s))
	   (res '())
	   (note (lambda (s) (set! res (cons s res)))))      
      (do-for (i k len)
	      (let ((ch (string-ref s i)))
		;;(format #t "ch = ~a ~%" ch)
		(cond
		 ((or (char=? ch #\<) (and (not (char=? ch #\<)) (= i (- len 1))))
		  (let* ((j (decode-bracket s (+ i 1)))
			 (sub (cond
			       ((= k 0) (substring s k i))
			       ((= i (- len 1))  (substring s (+ k 1) (+ i 1)))
			       (#t (substring s (+ k 1) i)))))
		    (set! k j)
		    (note sub))))))
      (reverse res))))


(define get-outside
  (lambda (s)
    (get-outside-helper s 0)))


(define recon
  (lambda (s)
    (let ((insides (get-inside s))
	  (outsides (get-outside s)))
      ;; (format #t "recon :~%")
      ;; (format #t "recon outsides : ~a ~%" outsides)
      ;; (format #t "recon insides  : ~a ~%" insides)      
      (let* ((res ""))
	(letrec ((foo (lambda ()
			(cond
			 ((null? outsides) #f)
			 (#t (set! res (string-append res (car outsides)))
			     ;; (format #t "res = ~a ~%" res)
			     (set! outsides (cdr outsides))
			     (cond
			      ((null? insides) #f)
			      (#t
			       (set! res (string-append res "<" (car insides) ">"))
			       (set! insides (cdr insides))
			       (foo))))))))
	  (foo)
	  res)))))


(define check
  (lambda (s)
    (let ((rec (recon s)))
    ;; (format #t "original : ~%[~a]~%" s)
    ;; (format #t "reconsti : ~%[~a]~%" rec)
    (let ((compare (equal? s rec)))
      ;; (format #t "same ? ~a~%" compare)
      compare))))

;; should return no failed outcomes - ie empty list
(define check-test
  (lambda ()
    (filter (lambda (x)
	      (bind (outcome str) x
		    (eq? outcome #f)))
	    (map (lambda (y) (list (check y) y)) input))))



  ;; (map (lambda (x)
  ;; 	 (irregex-replace/all "['[']" let* ((data1 (reader))
  ;; 	 (data2 (map (lambda (x) (string-append "" (irregex-replace/all "['[']" x "<"))) data1))
  ;; 	 (data3 (map (lambda (x) (irregex-replace/all "[']']" x ">")) data2))
  ;; 	 )
  ;;   (format #t "data2 ~a ~%" (car data2))
  ;;   (format #t "data3 ~a ~%" (car data3))    
  ;;   data3))


;; (irregex-replace/all "['[']" (irregex-replace/all "[']']" "hello[[[]]]world" ">") "hello[[[world" "<")


;; regex
;; hypernet seqeunces ...[XXX] ...
;; ^ mean start of string
;; $ mean end of string
;; 
;; ^XXX[
;; ]XXXX..[
;; ]XXX$
;;
;; partition XXXX into sequences of 4 characters
;; check if they are AB BA sequences , pair two different characters then both reverse of same two characters
;; XXXX is not an abba sequence because X and X are same character
;; XYXY is not because not reversed XY would reverse to YX giving correct XYYX

;; cannot figure out how put [ character into regexp
;;(string-match (regexp "([a-z])+") (car input))

;;(irregex-search (irregex "[srz.*])") (car input) 'single-line)

;; (string-split-fields (string->irregex "^([a-z]+)['[']") (car input) 'single-line)
;; (string-split-fields (string->irregex "['[']([a-z]+)[']']") (car input) 'single-line)
;; (string-split-fields (string->irregex "]([a-z]+)$") (car input) 'single-line)
;; 
;; (let ((ob (irregex-search (string->irregex "^([a-z]+)<") (car input))))
;;   (cond
;;    ((irregex-match-data? ob)
;;     
;; (irregex-search (string->irregex "<([a-z]+)>") (car input))
;; (irregex-search (string->irregex ">([a-z]+)$") (car input))
;;(car input)

;; for a string s - chop it into strings of length 4 abcdef => abcd ; bcde ; cdef ...

(define chop-strings3
  (lambda (s)
    (let ((len (string-length s))
	  (res '()))
      (do-for (i 0 (- len 2))
	      (let ((sub (substring s i (+ i 3))))
		(set! res (cons sub res))))
      (reverse res))))

(chop-strings3 "")
(chop-strings3 "a")
(chop-strings3 "ab")
(chop-strings3 "abc")
(chop-strings3 "abcd")
(chop-strings3 "abcde")
(chop-strings3 "abcdef")
(chop-strings3 "abcdefg")

(define chop-strings4
  (lambda (s)
    (let ((len (string-length s))
	  (res '()))
      (do-for (i 0 (- len 3))
	      (let ((sub (substring s i (+ i 4))))
		(set! res (cons sub res))))
      (reverse res))))

(chop-strings4 "")
(chop-strings4 "a")
(chop-strings4 "ab")
(chop-strings4 "abc")
(chop-strings4 "abcd")
(chop-strings4 "abcde")
(chop-strings4 "abcdef")
(chop-strings4 "abcdefg")



(define abba
  (lambda (s)
    (let ((len (string-length s)))
      (cond
       ((not (= len 4)) (error "abba only works strings length 4 "))
       (#t (let ((ch0 (string-ref s 0))
		 (ch1 (string-ref s 1))
		 (ch2 (string-ref s 2))
		 (ch3 (string-ref s 3)))
	     (and (not (char=? ch0 ch1))
		  (char=? ch0 ch3)
		  (char=? ch1 ch2))))))))

(define any-abba-inside? 
  (lambda (s)
    (let* ((fours (apply append (map chop-strings4 (get-inside s))))
	   (abbas (map (lambda (y) (list (abba y) y)) fours))
      	   (res (filter (lambda (x)
			 (bind (outcome str) x
			       (if outcome x #f)))
			abbas)))		       
      (cond
       ((null? res) #f)
       (#t res)))))

(define any-abba-outside? 
  (lambda (s)
    (let* ((fours (apply append (map chop-strings4 (get-outside s))))
	   (abbas (map (lambda (y) (list (abba y) y)) fours))
      	   (res (filter (lambda (x)
			 (bind (outcome str) x
			       (if outcome x #f)))
			abbas)))		       
      (cond
       ((null? res) #f)
       (#t res)))))


(define aba
  (lambda (s)
    (let ((len (string-length s)))
      (cond
       ((not (= len 3)) (error "aba only works strings length 3 "))
       (#t (let ((ch0 (string-ref s 0))
		 (ch1 (string-ref s 1))
		 (ch2 (string-ref s 2)))
	     (and (not (char=? ch0 ch1))
		  (char=? ch0 ch2))))))))



;; ------ chop strings 3 -- done
;; -------- aba 
(define any-aba-inside? 
  (lambda (s)
    (let* ((threes (apply append (map chop-strings3 (get-inside s))))
	   (abas (map (lambda (y) (list (aba y) y)) threes))
      	   (res (filter (lambda (x)
			 (bind (outcome str) x
			       (if outcome x #f)))
			abas)))		       
      (cond
       ((null? res) #f)
       (#t res)))))

(define any-aba-outside? 
  (lambda (s)
    (let* ((threes (apply append (map chop-strings3 (get-outside s))))
	   (abas (map (lambda (y) (list (aba y) y)) threes))
      	   (res (filter (lambda (x)
			 (bind (outcome str) x
			       (if outcome x #f)))
			abas)))		       
      (cond
       ((null? res) #f)
       (#t res)))))


;; (any-abba (list "abba" "abcd"))
;; (any-abba (list "abcd" "dabc"))

(define part-a
  (lambda ()
    (let* ((c 0)
	   (i 0)
	   (fun (lambda (s)
		  (format #t "examining string ~a ~%" i)
		  (set! i (+ i 1))
		  (format #t "~a~%" s)
		  (let* ((ins (any-abba-inside? s))
			 (outs (any-abba-outside? s)))
		    (format #t "insides ~a ~%" ins)
		    (format #t "outsides ~a ~%" outs)
		    (cond
		     ((and outs (not ins))
		      (set! c (+ c 1))))))))
      (map fun input)
      (format #t "there were ~a strings with outsides abba and no insides abba ~%" c)
      c)))


(define compatible-aba
  (lambda (s1 s2)
    (let ((len1 (string-length s1))
	  (len2 (string-length s2)))
      (cond
       ((or (not (= len1 3)) (not (= len2 3)))
	(error "compatible-aba only compare strings of length 3 "))
       (#t (let ((a0 (string-ref s1 0))
		 (a1 (string-ref s1 1))
		 (a2 (string-ref s1 2))
		 (b0 (string-ref s2 0))
		 (b1 (string-ref s2 1))
		 (b2 (string-ref s2 2))) 
	     ;; a0 a1 a2     a b a    a0 = b1 ; a2 = b1 ;  \/ shape
	     ;; b0 b1 b2     b a b    a1 = b0 ; a1 = b2 ; /\ shape 
	     (and (not (char=? a0 b0)) ; hedge against aba fail elsewhere
	          (char=? a0 b1)
		  (char=? a2 b1) ;; \/ shape
		  (char=? a1 b0)  ;; /\ shape
		  (char=? a1 b2))))))))

;;(compatible-aba "aba" "bab")
;;(compatible-aba "aaa" "bbb")


(define has-compatible-aba
  (lambda (ins outs)
    (call/cc (lambda (exit)
	       (do-list (str1 ins)
			(do-list (str2 outs)
				 (when (compatible-aba str1 str2)
				   (exit (list str1 str2)))))
	       #f))))


(define part-b
  (lambda ()
    (let* ((c 0)
	   (i 0)
	   (fun (lambda (s)
		  (format #t "examining string ~a ~%" i)
		  (set! i (+ i 1))
		  (format #t "~a~%" s)
		  (let* ((ins (let ((tmp (any-aba-inside? s))) (if tmp (map second tmp) #f)))
			 (outs (let ((tmp (any-aba-outside? s))) (if tmp (map second tmp) #f)))
			 (has (let ((tmp (and ins outs))) (if tmp (has-compatible-aba ins outs) #f)))
			 )
			 ;;(has (has-compatible-aba ins outs)))
		    (format #t "insides => [~a] ~%" ins)
		    (format #t "outsides => [~a] ~%" outs)
		    (format #t "has-compatible-aba ~a ~%" has)		    
		    (cond
		     (has ;; compatible? [aba in ins] with [bab in outs] ? 
		      (set! c (+ c 1))))))))
      (map fun input)
      (format #t "there were ~a strings with outsides abba and no insides abba ~%" c)
      c)))



#|

> (part-b)

...
...

examining string 1996 
vwpklmfpzssyoifbb<bwtpxodxedpdsvgu>azathmhdnqkhbmrdlxn
insides => [(dpd)] 
outsides => [(aza hmh)] 
has-compatible-aba #f 
examining string 1997 
iwzncixpjxypnmykke<wlhvfjbumhmtachoab>wtftbolwhpwnropnzv<zhfcmkbwizknymev>twrumqadwvsaciwbwoi<evodjtvrucrzgtwio>fmmbddkwwpaziycik
insides => [(mhm)] 
outsides => [(tft wbw)] 
has-compatible-aba #f 
examining string 1998 
dkodbaotlfdaphwzbcc<ldzeemqiovyqjgs>qxibabdusgaistkru<usglloxgycyynmp>aaocvclsocababbzxeg<liaacgfxytuqudp>jvvqsypuoduyhvraak
insides => [(ycy uqu)] 
outsides => [(bab cvc aba bab)] 
has-compatible-aba #f 
examining string 1999 
bwzsacxgqkbjycgfw<dbnligvrmqscasutn>rbgybqqsgjvlonkut
insides => [#f] 
outsides => [#f] 
has-compatible-aba #f 
there were 260 strings with outsides abba and no insides abba 
260

0.365s CPU time, 0.003s GC time (major), 883067/70209 mutations (total/tracked), 5/7486 GCs (major/minor), maximum live heap: 973.45 KiB

260 ............ 

|#




      



	      
