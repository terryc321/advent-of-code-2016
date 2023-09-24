
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
	  ;;(format #t "proper ~a ~%" word)
	  (format #t "~a : ~a~%" (shift-cypher (substring str 0 ni) id) id)
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

#|

clock arithmeticc modulo 26

a b c d e f g h i j k l m n o p q r s t u v w x y z
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5

26 total


|#
(define (shift-cypher str n)
  (let* ((len (string-length str))
	 (res (make-string len)))
    (letrec ((loop (lambda (i)
		     (cond
		      ((>= i len) #f)
		      (#t
		       (let ((ch (string-ref str i)))
			 (cond
			  ((char=? ch #\-) (string-set! res i #\space))
			  (#t (let ((k (modulo
				       (+ n (- (char->integer ch) (char->integer #\a)))
				       26)))
				(string-set! res i (integer->char (+ k (char->integer #\a))))
				#t))))
		       (loop (+ i 1))
		       )))))
      (loop 0)
      res)))






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

part I I

projectile magnetic jellybean training  : 644
cryogenic flower design  : 900
cryogenic bunny engineering  : 932
top secret candy coating design  : 798
biohazardous jellybean training  : 971
top secret international bunny marketing  : 532
colorful scavenger hunt design  : 564
magnetic rabbit reacquisition  : 595
colorful projectile dye technology  : 356
top secret scavenger hunt management  : 583
projectile chocolate engineering  : 621
biohazardous scavenger hunt purchasing  : 671
unstable biohazardous plastic grass management  : 129
colorful rabbit sales  : 939
radioactive flower design  : 616
classified egg financing  : 414
colorful candy operations  : 784
classified candy coating containment  : 511
radioactive flower storage  : 521
corrosive flower management  : 394
classified dye development  : 658
consumer grade egg workshop  : 438
cryogenic dye management  : 173
rampaging radioactive jellybean technology  : 673
military grade egg user testing  : 292
classified cryogenic plastic grass logistics  : 226
consumer grade rabbit user testing  : 595
projectile dye customer service  : 608
military grade rabbit research  : 742
rampaging basket logistics  : 190
cryogenic candy reacquisition  : 855
radioactive chocolate development  : 614
projectile flower reacquisition  : 232
magnetic chocolate marketing  : 407
consumer grade scavenger hunt deployment  : 899
military grade candy management  : 373
biohazardous weaponized scavenger hunt analysis  : 244
classified egg purchasing  : 395
unstable rabbit acquisition  : 351
fuzzy dye acquisition  : 459
cryogenic plastic grass reacquisition  : 189
military grade egg workshop  : 977
radioactive rabbit management  : 678
top secret weaponized candy logistics  : 217
unstable plastic grass reacquisition  : 891
radioactive flower operations  : 498
military grade scavenger hunt workshop  : 571
radioactive candy reacquisition  : 154
radioactive bunny research  : 530
unstable egg analysis  : 214
corrosive dye purchasing  : 769
fuzzy jellybean receiving  : 558
radioactive chocolate operations  : 187
biohazardous rabbit customer service  : 889
magnetic bunny design  : 817
biohazardous candy customer service  : 544
rampaging rabbit financing  : 384
classified radioactive scavenger hunt storage  : 346
top secret basket design  : 623
rampaging basket engineering  : 183
projectile rabbit purchasing  : 454
cryogenic scavenger hunt storage  : 697
rampaging flower marketing  : 959
radioactive basket financing  : 507
fuzzy basket laboratory  : 103
weaponized bunny deployment  : 197
magnetic jellybean financing  : 566
top secret scavenger hunt laboratory  : 531
fuzzy scavenger hunt engineering  : 892
unstable jellybean user testing  : 472
magnetic basket financing  : 643
magnetic projectile scavenger hunt customer service  : 833
colorful military grade candy coating management  : 743
cryogenic bunny development  : 719
weaponized magnetic plastic grass workshop  : 634
classified chocolate reacquisition  : 774
military grade colorful scavenger hunt customer service  : 241
top secret plastic grass services  : 525
projectile cryogenic candy coating training  : 535
colorful flower management  : 826
classified egg laboratory  : 201
rampaging chocolate containment  : 396
rampaging dye financing  : 524
rampaging scavenger hunt financing  : 761
fuzzy candy marketing  : 587
radioactive rabbit acquisition  : 451
weaponized jellybean deployment  : 534
classified scavenger hunt training  : 892
military grade jellybean receiving  : 671
corrosive fuzzy bunny receiving  : 751
weaponized candy research  : 649
colorful basket customer service  : 314
biohazardous flower receiving  : 476
international bunny workshop  : 528
top secret fuzzy rabbit purchasing  : 838
corrosive jellybean services  : 795
international biohazardous egg reacquisition  : 516
international bunny research  : 606
colorful plastic grass acquisition  : 741
consumer grade dye acquisition  : 566
consumer grade dye operations  : 960
radioactive egg acquisition  : 946
fuzzy chocolate receiving  : 614
projectile chocolate sales  : 840
classified egg services  : 258
military grade candy coating reacquisition  : 526
classified flower logistics  : 441
colorful dye logistics  : 665
weaponized dye development  : 472
biohazardous fuzzy bunny research  : 857
weaponized egg deployment  : 100
radioactive candy coating customer service  : 134
fuzzy jellybean analysis  : 807
military grade candy coating department  : 418
military grade biohazardous bunny laboratory  : 905
unstable flower shipping  : 485
classified flower acquisition  : 316
colorful candy coating engineering  : 986
international egg operations  : 859
corrosive candy coating research  : 586
fuzzy basket department  : 649
corrosive candy coating reacquisition  : 314
rampaging chocolate purchasing  : 860
consumer grade dye management  : 550
colorful basket receiving  : 930
top secret chocolate logistics  : 466
classified egg management  : 446
international plastic grass reacquisition  : 610
rampaging egg reacquisition  : 819
magnetic basket sales  : 241
consumer grade jellybean technology  : 542
cryogenic egg laboratory  : 493
cryogenic egg receiving  : 897
military grade rabbit department  : 200
weaponized dye customer service  : 336
projectile candy coating reacquisition  : 229
classified bunny shipping  : 658
top secret candy coating analysis  : 199
weaponized flower training  : 512
northpole object storage  : 501  <<<<<<<<<<<<<<<---------------- room 501
international jellybean marketing  : 449
radioactive flower shipping  : 751
biohazardous basket workshop  : 282
rampaging flower deployment  : 428
cryogenic dye customer service  : 732
fuzzy basket marketing  : 968
fuzzy scavenger hunt storage  : 353
corrosive rabbit user testing  : 110
biohazardous candy training  : 242
colorful scavenger hunt deployment  : 508
weaponized international basket laboratory  : 585
international bunny deployment  : 365
international flower analysis  : 203
biohazardous rabbit engineering  : 170
fuzzy rabbit user testing  : 659
weaponized dye purchasing  : 582
military grade jellybean engineering  : 927
weaponized bunny laboratory  : 227
projectile flower development  : 959
radioactive candy coating services  : 723
radioactive candy coating research  : 533
rampaging rabbit user testing  : 139
consumer grade dye shipping  : 250
radioactive rabbit financing  : 783
biohazardous dye containment  : 851
biohazardous egg development  : 897
fuzzy radioactive basket department  : 679
cryogenic colorful dye research  : 604
weaponized magnetic rabbit analysis  : 649
fuzzy jellybean acquisition  : 207
corrosive flower storage  : 197
top secret jellybean containment  : 748
cryogenic rabbit development  : 662
unstable basket customer service  : 471
international dye research  : 262
rampaging rabbit management  : 667
biohazardous jellybean marketing  : 183
top secret flower sales  : 919
biohazardous chocolate development  : 245
weaponized jellybean research  : 738
fuzzy bunny financing  : 566
consumer grade candy storage  : 921
rampaging rabbit development  : 160
weaponized basket logistics  : 730
rampaging rabbit workshop  : 608
biohazardous cryogenic rabbit user testing  : 367
radioactive dye storage  : 431
classified plastic grass storage  : 643
fuzzy egg engineering  : 839
biohazardous flower training  : 918
international rabbit management  : 903
top secret plastic grass logistics  : 316
top secret egg design  : 856
rampaging chocolate storage  : 965
military grade jellybean training  : 863
top secret egg acquisition  : 143
international chocolate research  : 162
magnetic plastic grass workshop  : 852
fuzzy chocolate user testing  : 109
magnetic unstable bunny technology  : 326
cryogenic dye user testing  : 591
radioactive plastic grass deployment  : 341
top secret bunny training  : 150
projectile candy design  : 183
unstable jellybean purchasing  : 720
military grade candy coating shipping  : 114
weaponized jellybean purchasing  : 695
magnetic unstable candy coating research  : 313
weaponized chocolate technology  : 631
classified rabbit training  : 238
international candy design  : 263
radioactive egg storage  : 266
unstable scavenger hunt development  : 129
cryogenic candy coating department  : 321
biohazardous basket department  : 874
weaponized candy coating services  : 604
consumer grade basket logistics  : 996
radioactive flower laboratory  : 411
corrosive basket customer service  : 299
rampaging chocolate user testing  : 336
fuzzy basket purchasing  : 925
classified rabbit user testing  : 279
colorful jellybean design  : 895
consumer grade flower technology  : 801
cryogenic candy coating acquisition  : 185
cryogenic candy storage  : 308
magnetic basket development  : 641
classified chocolate training  : 595
corrosive scavenger hunt operations  : 894
corrosive jellybean deployment  : 556
corrosive scavenger hunt research  : 163
cryogenic dye reacquisition  : 303
international candy financing  : 845
fuzzy top secret plastic grass containment  : 997
biohazardous basket engineering  : 748
colorful candy laboratory  : 989
cryogenic dye workshop  : 215
fuzzy plastic grass operations  : 124
fuzzy egg logistics  : 138
unstable candy containment  : 980
projectile scavenger hunt engineering  : 732
magnetic scavenger hunt research  : 693
classified cryogenic basket logistics  : 212
fuzzy rabbit design  : 639
consumer grade basket technology  : 472
rampaging rabbit storage  : 608
unstable classified flower user testing  : 887
military grade candy coating training  : 788
fuzzy plastic grass user testing  : 273
radioactive plastic grass analysis  : 112
biohazardous jellybean development  : 600
rampaging chocolate workshop  : 283
top secret candy coating training  : 265
international candy logistics  : 298
137896

|#
