
#|

code designed for the new computer you just assembled. You'll have to execute the code and get the password.

The assembunny code you've extracted operates on four registers (a, b, c, and d)
that start at 0 and can hold any integer.

However, it seems to make use of only a few instructions:

cpy x y copies x (either an integer or the value of a register) into register y.

inc x increases the value of register x by one.

dec x decreases the value of register x by one.

jnz x y jumps to an instruction y away (positive means forward; negative means backward),
but only if x is not zero.

The jnz instruction moves relative to itself:
an offset of -1 would continue at the previous instruction,
while an offset of 2 would skip over the next instruction.

For example:

cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a

The above code would set register a to 41,
increase its value by 2, decrease its value by 1,
and then skip the last dec a
(because a is not zero, so the jnz a 2 skips it),
leaving register a at 42.
When you move past the last instruction, the program halts.

After executing the assembunny code in your puzzle input,
what value is left in register a?

|#

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
  (with-input-from-file "day12/input"
    (lambda ()
      (read))))

(set! input (list->vector input))

(define ip 0)

(define ip-len (vector-length input))

;; part I I -- set c to be value of ignition key 1
(define regs '((a 0)(b 0)(c 1)(d 0)))

(define (cpy-reg! val reg)
  (cond
   ((integer? val)
    (set-car! (cdr (assoc reg regs)) val)
    (set! ip (+ ip 1)))
   ((member val '(a b c d))
    (set-car! (cdr (assoc reg regs)) (second (assoc val regs)))
    (set! ip (+ ip 1)))
   (#t (error "set-reg!" (list 'value val 'dest-register reg)))))

(define (inc-reg! reg)
  (cond
   ((member reg '(a b c d))
    (set-car! (cdr (assoc reg regs)) (+ 1 (second (assoc reg regs))))
    (set! ip (+ ip 1)))
   (#t (error "inc-reg!" (list 'dest-register reg)))))

(define (dec-reg! reg)
  (cond
   ((member reg '(a b c d))
    (set-car! (cdr (assoc reg regs)) (+ -1 (second (assoc reg regs))))
    (set! ip (+ ip 1)))
   (#t (error "dec-reg!" (list 'dest-register reg)))))

(define (jnz reg dist)
  ;;(format #t "jnz test : reg = ~a : distance = ~a ~%" reg dist)
  (cond
   ((integer? reg)
    (cond
     ((zero? reg) (set! ip (+ ip 1)))
     (#t (set! ip (+ ip dist)))))
   ((member reg '(a b c d))
    (let ((reg-val (second (assoc reg regs))))
      (cond
       ((zero? reg-val) (set! ip (+ ip 1)))
       (#t (set! ip (+ ip dist))))))
   (#t (error "jnz!" (list 'reg reg 'distance dist)))))





(define (foo code)
  (cond
   ((or (< ip 0) (>= ip ip-len))
    'halt)
   (#t (let ((instr (vector-ref code ip)))
	 (match instr
	   [('cpy ri r)
	    (cpy-reg! ri r)
	    ;;(format #t "~a : matched CPY reg-or-immediate ri = ~a : r = ~a ~%" instr ri r)
	    ]
	   [('inc r)
	    (inc-reg! r)
	    ;;(format #t "~a : matched INC r = ~a ~%" instr r)
	    ]
	   [('dec r)
	    (dec-reg! r)
	    ;;(format #t "~a : matched DEC r = ~a ~%" instr r)
	    ]
	   [('jnz ri r)
	    (jnz ri r)
	    ;;(format #t "~a : matched JNZ ri = ~a : r = ~a ~%" instr ri r)
	    ]
	   [ _ (format #t "~a : matched GARBAGE ~%" x) (error "GARBAGE MATCH!" (list instr ))])
	 (foo code)
	 ))))




#|

results
------------
#;1211> regs
((a 0) (b 0) (c 0) (d 0))
#;1213> ,t (foo input)
4.66s CPU time, 0.034s GC time (major), 14756681/97276 mutations (total/tracked), 29/52840 GCs (major/minor), maximum live heap: 935.72 KiB
halt
#;1216> regs
((a 318009) (b 196418) (c 0) (d 0))
#;1219> 

register a has value 318009 after computation

------------------------------------

part I I

#;1219> regs
((a 0) (b 0) (c 1) (d 0))
#;1228> ,t (foo input)
133.199s CPU time, 0.936s GC time (major), 428139410/2823321 mutations (total/tracked), 833/1533023 GCs (major/minor), maximum live heap: 936.15 KiB
halt
#;1230> regs
((a 9227663) (b 5702887) (c 0) (d 0))
#;1232>

register a has value 9227663 after computation



|#


  
  

;; already structural lisp code so not string pattern matching
;; only four registers a b c d
#|
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
|#

