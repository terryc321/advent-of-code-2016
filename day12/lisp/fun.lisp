(defpackage :aoc
  (:use :cl))

(in-package :aoc)

(defun != (x y) (not (= x y)))

(defun run (a b c d)
  (labels
      ((v0 () (setq a 1) (v1))
       (v1 () (setq b 1) (v2))
       (v2 () (setq d 26) (v3))
       (v3 () (if (!= c 0) (v5) (v4)))
       (v4 () (v9))
       (v5 () (setq c 7) (v6))
       (v6 () (incf d) (v7))
       (v7 () (decf c) (v8))
       (v8 () (if (!= c 0) (v6) (v9)))
       (v9 () (setq c a) (v10))
       (v10 () (incf a) (v11))
       (v11 () (decf b) (v12))
       (v12 () (if (!= b 0) (v10) (v13)))
       (v13 () (setq b c)(v14))
       (v14 () (decf d)(v15))
       (v15 () (if (!= d 0) (v9) (v16)))
       (v16 () (setq c 18) (v17))
       (v17 () (setq d 11) (v18))
       (v18 () (incf a)(v19))
       (v19 () (decf d)(v20))
       (v20 () (if (!= d 0) (v18) (v21)))
       (v21 () (decf c) (v22))
       (v22 () (if (!= c 0) (v17) (vDone)))
       (vDone () t))
    (v0)
    (format t "--------------------------~%")
    (format t "a = ~a~%" a)
    (format t "b = ~a~%" b)
    (format t "c = ~a~%" c)
    (format t "d = ~a~%" d)
    (format t "--------------------------~%")))

(run 0 0 0 0)

(run 0 0 1 0)

#|
* (time (load "fun.lisp"))
--------------------------
a = 318009
b = 196418
c = 0
d = 0
--------------------------
--------------------------
a = 9227663
b = 5702887
c = 0
d = 0
--------------------------
Evaluation took:
  0.036 seconds of real time
  0.038277 seconds of total run time (0.038256 user, 0.000021 system)
  105.56% CPU
  37 forms interpreted
  62 lambdas converted
  141,125,476 processor cycles
  1,274,992 bytes consed


|#
