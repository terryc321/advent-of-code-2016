
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
  (with-input-from-file "day8/input"
    (lambda ()
      (read))))


(define (foo)
  (letrec ((bar (lambda (str)
		  (let ((m1 (string-match "rect ([0-9]+)x([0-9]+)" str))
			(m2 (string-match "rotate row y=([0-9]+) by ([0-9]+)" str))
			(m3 (string-match "rotate column x=([0-9]+) by ([0-9]+)" str)))
		    (cond
		     (m1 (rect (string->number (second m1)) (string->number (third m1))))
		     (m2 (rotate-row (string->number (second m2)) (string->number (third m2))))
		     (m3 (rotate-col (string->number (second m3)) (string->number (third m3))))
		     (#t (error "foo.bar" (list str "not recognised"))))))))
    (for (lambda (x)
	   ;;(format #t "processing [~a] ~%" x)
	   (bar x))
	 input)))



(define board #f)


(define (make-board)
  (set! board (make-vector 6))
  (vector-set! board 0 (make-vector 50 #f))
  (vector-set! board 1 (make-vector 50 #f))
  (vector-set! board 2 (make-vector 50 #f))
  (vector-set! board 3 (make-vector 50 #f))
  (vector-set! board 4 (make-vector 50 #f))
  (vector-set! board 5 (make-vector 50 #f)))


(make-board)

  

(define (set-xy! x y v)
  (let ((row (vector-ref board y)))
    (vector-set! row x v)))

(define (get-xy x y)
  (let ((row (vector-ref board y)))
    (vector-ref row x)))

(define (rect a b)
  (letrec ((loop (lambda (x y)
		   (cond
		    ((>= y b) #f)
		    ((>= x a) (loop 0 (+ y 1)))
		    (#t (set-xy! x y #t)
			;;(format #t "setting ~a ~a ~%" x y)
			(loop (+ x 1) y))))))    
    ;;(format #t "making rect ~a x ~a ~%" a b)
    (loop 0 0)))


;; 0 to 49
(define (rotate-row y n)
  (letrec ((rot (lambda (i)
		  (cond
		   ((<= i 0) #f)
		   (#t
		    (let ((end (get-xy 49 y)))
		      (map (lambda (x)
			     (let ((tmp (get-xy x y)))
			       ;;(format #t "rot-row: setting ~a ~a => ~a ~a ~%" x y (+ x 1) y)
			       (set-xy! (+ x 1) y tmp)))
			   (reverse (iota 49)))
		      (set-xy! 0 y end)
		      (rot (- i 1))))))))
    ;;(format #t "rotating row y = ~a by  ~a ~%" y n)
    (rot n)))


(define (test)
  (vector-set! board 0 (list->vector (iota 50)))
  (set-xy! 0 0 0)
  (set-xy! 0 1 1)
  (set-xy! 0 2 2)
  (set-xy! 0 3 3)
  (set-xy! 0 4 4)
  (set-xy! 0 5 5))

(define (show-board)
  (format #t "~%")
  (format #t "~a~%" (vector-ref board 0))
  (format #t "~a~%" (vector-ref board 1))
  (format #t "~a~%" (vector-ref board 2))
  (format #t "~a~%" (vector-ref board 3))
  (format #t "~a~%" (vector-ref board 4))
  (format #t "~a~%" (vector-ref board 5)))

  

(define (rotate-col x n)
  (letrec ((rot (lambda (i)
		  (cond
		   ((<= i 0) #f)
		   (#t
		    (let ((end (get-xy x 5)))
		      (map (lambda (y)
			     (let ((tmp (get-xy x y)))
			       ;;(format #t "rot-row: setting ~a ~a => ~a ~a ~%" x y (+ x 1) y)
			       (set-xy! x (+ y 1) tmp)))
			   (reverse (iota 5)))
		      (set-xy! x 0 end)
		      (rot (- i 1))))))))
    ;;(format #t "rotating row y = ~a by  ~a ~%" y n)
    ;;(format #t "rotating column x = ~a by  ~a ~%" x n)
    (rot n)))

  


#|
(string-match "rect ([0-9]+)x([0-9]+)" "rect 1x1")
;;("rect 1x1" "1" "1")

(string-match "rotate row y=([0-9]+) by ([0-9]+)" "rotate row y=0 by 5")
;;("rotate row y=0 by 5" "0" "5")

(string-match "rotate column x=([0-9]+) by ([0-9]+)" "rotate column x=0 by 1")
;;("rotate column x=0 by 1" "0" "1")
|#

(define (count-pixels)
  (letrec ((loop (lambda (x y tot)
		   (cond
		    ((>= y 6) tot)
		    ((>= x 50) (loop 0 (+ y 1) tot))
		    (#t
		     ;;(format #t "counting pixel ~a ~a ~%" x y)
		     (cond
		      ((get-xy x y)
		       (loop (+ x 1) y (+ tot 1)))
		      (#t
		       (loop (+ x 1) y tot))))))))
    (loop 0 0 0)))


(define (draw-pixels)
  (format #t "~%")
  (letrec ((loop (lambda (x y tot)
		   (cond
		    ((>= y 6) tot)
		    ((>= x 50)
		     (format #t "~%")
		     (loop 0 (+ y 1) tot))
		    (#t
		     ;;(format #t "counting pixel ~a ~a ~%" x y)
		     (cond
		      ((get-xy x y)
		       (format #t "#")
		       (loop (+ x 1) y (+ tot 1)))
		      (#t
		       (format #t ".")
		       (loop (+ x 1) y tot))))))))
    (loop 0 0 0)
    (format #t "~%")))

#|
#;1649> (draw-pixels)

####.####.#..#.####..###.####..##...##..###...##..
...#.#....#..#.#....#....#....#..#.#..#.#..#.#..#.
..#..###..####.###..#....###..#..#.#....#..#.#..#.
.#...#....#..#.#.....##..#....#..#.#.##.###..#..#.
#....#....#..#.#.......#.#....#..#.#..#.#....#..#.
####.#....#..#.#....###..#.....##...###.#.....##..

looks like  "ZFHFSFOGPO"

accepted answer

|#



(define (solve)
  (make-board)
  (foo)
  (count-pixels))

#|

(solve)
....
....
119

should be 119 pixels lit

accepted answer

|#







