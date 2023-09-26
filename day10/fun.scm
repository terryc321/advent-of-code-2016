
(import scheme)
(import (chicken format))
(import (chicken sort))

(import procedural-macros)
(import regex)

;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))

(import sequences)

(import srfi-1)

(define input
  (with-input-from-file "day10/input"
    (lambda ()
      (read))))

(define bots '())
(define outputs '())

(define (foo)
  (letrec ((bar (lambda (str)
		  (let ((m1 (string-match "bot ([0-9]+) gives low to bot ([0-9]+) and high to bot ([0-9]+)" str))
			(m2 (string-match "bot ([0-9]+) gives low to output ([0-9]+) and high to bot ([0-9]+)" str))
			(m3 (string-match "bot ([0-9]+) gives low to output ([0-9]+) and high to output ([0-9]+)" str))
			(m4 (string-match "value ([0-9]+) goes to bot ([0-9]+)" str)))
		    (cond
		     (m1 (case1 m1))
		     (m2 (case2 m2))
		     (m3 (case3 m3))
		     (m4 (case4 m4))
		     (#t (error "foo.bar" (list str "not recognised"))))))))
    ;; for iterator
    (for (lambda (x)
	   (format #t "processing [~a] ~%" x)
	   (bar x))
	 input)))


#|
lets have a macro for this 
    (when (not (member b bots))
      (set! bots (cons b bots)))
|#
(define-syntax when-not
  (syntax-rules ()
    ((_ n xs)
     (when (not (member n xs))
       (set! xs (cons n xs))))))

(define-syntax i2
  (syntax-rules ()
    ((_ xs)
     (string->number (second xs)))))

(define-syntax i3
  (syntax-rules ()
    ((_ xs)
     (string->number (third xs)))))

(define-syntax i4
  (syntax-rules ()
    ((_ xs)
     (string->number (fourth xs)))))


;; what bots
;; what outputs
(define (case1 m)
  (let ((b (i2 m))
	(lo (i3 m))
	(hi (i4 m)))
    ;;(format #t "case1.match = ~a ~%" m)
    (format #t "bot ~a gives low to ~a and high to bot ~a ~%" b lo hi)
    (when-not b bots)
    (when-not lo bots)
    (when-not hi bots)
    ))


(define (case2 m)
  (let ((b (i2 m))
	(lo (i3 m))
	(hi (i4 m)))
  ;;(format #t "case2.match = ~a ~%" m)
    (format #t "bot ~a gives low to output ~a and high to bot ~a ~%" b lo hi)
    (when-not b bots)
    (when-not lo outputs)
    (when-not hi bots)
  ))

(define (case3 m)
  (let ((b (i2 m))
	(lo (i3 m))
	(hi (i4 m)))  
  ;;(format #t "case3.match = ~a ~%" m)
    (format #t "bot ~a gives low to output ~a and high to output ~a ~%" b lo hi)
    (when-not b bots)
    (when-not lo outputs)
    (when-not hi outputs)
  ))

(define (case4 m)
  (let ((v (i2 m))
	(b (i3 m)))
  ;;(format #t "case4.match = ~a ~%" m)
    (format #t "value ~a goes to bot ~a ~%" v b)
    (when-not b bots)
  ))


#|
(string-match "value ([0-9)+) goes to bot ([0-9]+)" "value 23 goes to bot 138")

(foo)
collect data from run in bots and outputs

#;542> (sort bots <)
(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209)
#;548> (sort outputs <)
(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
#;554> (iota 210)
(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209)
#;559> (equal? (sort bots <) (iota 210))
#t
#;571> (equal? (sort outputs <) (iota 21))
#t
#;576>

210 bots
21 outputs

;; generates defines for all 210 bots and 21 outputs

|#

(define code '())  ;; 
(define code2 '()) ;; assignments code2
(define code3 '()) ;; definitions 

(define (fiz)
  (let ((rs '()))
    (for (lambda (x)
	   (set! rs (cons `(define ,(string->symbol (format #f "bot~a" x)) '())
			  rs)))
	 (iota 210))
    (for (lambda (x)
	   (set! rs (cons `(define ,(string->symbol (format #f "output~a" x)) '())
			  rs)))
	 (iota 21))
    (cons 'begin (reverse rs))))


(set! code3 (fiz))



#|
make foo again , this time call g-case code generator for each case
each g-case will add its own bit of code onto front of code
when done , reverse it all
|#

(define (fuz)
  (set! code '())
  (letrec ((bar (lambda (str)
		  (let ((m1 (string-match "bot ([0-9]+) gives low to bot ([0-9]+) and high to bot ([0-9]+)" str))
			(m2 (string-match "bot ([0-9]+) gives low to output ([0-9]+) and high to bot ([0-9]+)" str))
			(m3 (string-match "bot ([0-9]+) gives low to output ([0-9]+) and high to output ([0-9]+)" str))
			(m4 (string-match "value ([0-9]+) goes to bot ([0-9]+)" str)))
		    (cond
		     (m1 (g-case1 m1))
		     (m2 (g-case2 m2))
		     (m3 (g-case3 m3))
		     (m4 (g-case4 m4))
		     (#t (error "fuz.bar" (list str "not recognised"))))))))
    ;; for iterator
    (for (lambda (x)
	   ;;(format #t "processing [~a] ~%" x)
	   (bar x))
	 input)))


#|
macro
(string->symbol (format #f "bot~a" lo))
|#
(define-syntax bot
  (syntax-rules ()
    ((_ n)
     (string->symbol (format #f "bot~a" n)))))

(define-syntax out
  (syntax-rules ()
    ((_ n)
     (string->symbol (format #f "output~a" n)))))


#|
			(when (and (= (apply min ,(bot b)) 17)
				   (= (apply max ,(bot b)) 61))
			  (format #t "61-17-bot is * ~a *~%" b))

|#
(define-syntax bot-61-17
  (syntax-rules ()
    ((_ b)
     (when (and (= (apply min (bot b)) 17)
		(= (apply max (bot b)) 61))
       (format #t "61-17-bot is * ~a *~%" b)))))
     


;; what bots
;; what outputs
(define (g-case1 m)
  (let ((b (i2 m))
	(lo (i3 m))
	(hi (i4 m)))
    ;;(format #t "case1.match = ~a ~%" m)
    (format #t "bot ~a gives low to ~a and high to bot ~a ~%" b lo hi)
    (set! code (cons `(when (= 2 (length ,(bot b)))
			;;,(bot-61-17 b)

			(when (and (= (apply min ,(bot b)) 17)
				   (= (apply max ,(bot b)) 61))
			  (format #t "61-17-bot is * ~a *~%" ,b))
			
			(set! ,(bot lo) (cons (apply min ,(bot b)) ,(bot lo)))
			(set! ,(bot hi) (cons (apply max ,(bot b)) ,(bot hi)))
			(set! ,(bot b) '())
			
			)
		     code))))


(define (g-case2 m)
  (let ((b (i2 m))
	(lo (i3 m))
	(hi (i4 m)))
    ;;(format #t "case2.match = ~a ~%" m)
    (format #t "bot ~a gives low to output ~a and high to bot ~a ~%" b lo hi)
    (set! code (cons `(when (= 2 (length ,(bot b)))
			;;,(bot-61-17 b)

			(when (and (= (apply min ,(bot b)) 17)
				   (= (apply max ,(bot b)) 61))
			  (format #t "61-17-bot is * ~a *~%" ,b))

			
			(set! ,(out lo)  (cons (apply min ,(bot b)) ,(out lo)))
			(set! ,(bot hi)  (cons (apply max ,(bot b)) ,(bot hi)))
			(set! ,(bot b) '())
			
			)
		     code))))

(define (g-case3 m)
  (let ((b (i2 m))
	(lo (i3 m))
	(hi (i4 m)))  
    ;;(format #t "case3.match = ~a ~%" m)
    (format #t "bot ~a gives low to output ~a and high to output ~a ~%" b lo hi)
    (set! code (cons `(when (= 2 (length ,(bot b)))
			;;,(bot-61-17 b)

			(when (and (= (apply min ,(bot b)) 17)
				   (= (apply max ,(bot b)) 61))
			  (format #t "61-17-bot is * ~a *~%" ,b))

			
			(set! ,(out lo) (cons (apply min ,(bot b)) ,(out lo)))
			(set! ,(out hi) (cons (apply max ,(bot b)) ,(out hi)))
			(set! ,(bot b) '())
			
			)
		     code))))

(define (g-case4 m)
  (let ((v (i2 m))
	(b (i3 m)))
    ;;(format #t "case4.match = ~a ~%" m)
    (format #t "value ~a goes to bot ~a ~%" v b)
    (set! code2 (cons `(set! ,(bot b) (cons ,v ,(bot b)))
		      code2))))


(fuz)

(define generated
  (append code3
	  code2
	  code))

;; ,p generated








