
(defpackage :fun
  (:use :cl))
(in-package :fun)


#|

The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.

As a diagram (F# for a Floor number, E for Elevator, H for Hydrogen, L for Lithium, M for Microchip, and G for Generator), the initial state looks like this:

F4 .  .  .  .  .  
F3 .  .  .  LG .  
F2 .  HG .  .  .  
F1 E  .  HM .  LM 

representation of state 
'((e hm lm)(hg)(lg)())

set of states done

set of states pending

start state = initial state
say this is start state
'((e hm lm)(hg)(lg)())

 e hm hg lm lg 
(1 1   2  1  3)
elevator on 1 , hm on 1 , hg on 2 , lm on 1 , lg on 3 

bootstrap process
working has ((1 1 2 1 3))

is the current state already been exhausted ?

find all reachable states from current state

rather for each reachable state from current-state is it      

|#

(defun next-state (s)
  nil
  )













