

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

(import matchable)


(define input
  (with-input-from-file "day13/input"
    (lambda ()
      (read))))



#|
					;
You arrive at the first floor of this new building to discover a much less welcoming environment than the shiny atrium of the last one. Instead, you are in a maze of twisty little cubicles, all alike. ;
					;
Every location in this area is addressed by a pair of non-negative integers (x,y). Each such coordinate is either a wall or an open space. You can't move diagonally. The cube maze starts at 0,0 and seems to extend infinitely toward positive x and y; negative values are invalid, as they represent a location outside the building. You are in a small waiting area at 1,1. ;
					;
While it seems chaotic, a nearby morale-boosting poster explains, the layout is actually quite logical. You can determine whether a given x,y coordinate will be a wall or an open space using a simple system: ;
					;
Find x*x + 3*x + 2*x*y + y + y*y.	;
Add the office designer's favorite number (your puzzle input). ;
part I puzzle input 1352		;
					; ;
					; ; ; ; ;
Find the binary representation of that sum; count the number of bits that are 1. ; ; ; ;
If the number of bits that are 1 is even, it's an open space. ;
If the number of bits that are 1 is odd, it's a wall. ;
|#


(define (formula x y)
  "x*x + 3*x + 2*x*y + y + y*y."
  (+ (* x x)
     (* 3 x)
     (* 2 x y)
     y
     (* y y)))

(define (puzzle-input) 1352)
;;(define (puzzle-input) 10)



;; int -> string  0s' 1's
(define (convert-binary n)
  (format #f "~b" n))


(define (count-1s s)
  (let ((len (string-length s)))
    (letrec ((foo (lambda (i n)
		    (cond
		     ((>= i len) n)
		     (#t (foo (+ i 1) (+ n (if (char=? (string-ref s i) #\1) 1 0))))))))
      (foo 0 0))))


;; decimal to binary conversion - through format print
;;(format #f "~b" 10000)
;;(format #f "~b" 65535)
(define (wall? x y)
  (let ((tot (count-1s (convert-binary (+ (puzzle-input) (formula x y))))))
    (cond
     ((odd? tot) #t)
     (#t #f))))


;; know from problem definition 31,39 does not have a wall on it
;; as it is our target
(define (show-board sx sy)
  (format #t "~%~%")
  (letrec ((foo (lambda (x y)
		  (format #t " ")
		  (cond
		   ((and (= x 31) (= y 39))
		    (format #t "!")
		    (foo (+ x 1) y))
		   ((> y sy) #f)
		   ((> x sx)
		    (format #t "~%")
		    (foo 0 (+ y 1)))
		   ((wall? x y)
		    (format #t "#")
		    (foo (+ x 1) y)
		    )
		   (#t
		    (format #t "_")
		    (foo (+ x 1) y))))))
    (foo 0 0)
    (format #t "~%~%")
    ))





#|
emacs editor				; ; ; ; ; ; ;
M-x toggle-truncate-lines		; ; ; ; ; ; ;
					; ; ; ; ; ; ;
shows entire line without auto wrap	; ; ; ; ; ; ;
					; ; ; ; ; ; ;

M-x occur
O
shows the solution route
90 occurrences of letter O

target marked as !
first O start on is counted as a step
but ! is not counted as a step so it corrects itself
take solution as 90 steps to reach 31,37 starting from 1,1 

part I I

reachability
can move up , left , right , down

       start at 1,1
            0,1   2,1   1,0  1,2
       if those locations are not then they get added to next 
|#

(define (valid? x y)
  (and (>= x 0) (>= y 0) (not (wall? x y))))


;; (define (reach )
;;   (let ((x 1) (y 1))
;;     (when (valid? (- x 1) y)  )
;;     )
;;   )

