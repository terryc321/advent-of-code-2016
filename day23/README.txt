
solution.scm

chicken/part-a.scm - solved almost instantly with a set to 7
result 
[13468] b[1] c[0] d[0]

real	0m0.006s
user	0m0.006s
sys	0m0.000s

chicken/part-b.scm -  brute force with a set to 12
something about multiply

a[479010028] b[1] c[0] d[0]

real	2m31.163s
user	2m30.595s
sys	0m0.548s

solved !

part - a :   13468      ........ ACCEPTED answer .
part - b :   479010028  ...... ACCEPTED answer .

--------------------------------------------------------------------------

Lessons learned ...
hygienic macros make an absolute pig of writing scheme macros

schism between compile time and runtime , where at compile time nothing is available
as a language besides CAR CDR some other limited things
makes writing macros painful

generated code is mangled by hygienic mechanism so more difficult to read
the generated code

the generated code is not portable to other languages , cannot see correct
translation


---------------------------------------------------------------------------

PROGRAM as a VECTOR of CLOSURES
-------------------------------
program represented as vector with closures

closures have access to global variables a , b , c , d , i instruction pointer

fvec 0 : f0
f0 = (lambda () if toggle[0] is even then v0a () else v0b ()
v0a = CPY A B
v0b = JNZ A B

compile time jump instruction not all known
--------------------------------------------
if cannot be determined at compile time what the jump destination is then
procedure just returns after setting i
i = 18 : (jnz 1 c)  since 1 is nonzero then jump to location i = i + c
since we do not know value of c at compile time we just return after setting i = i + c

the outer loop run will repeatedly keep retrieving closure at index i , then calling it
as long as index i >= 0 and i <= 25 length of the program is 26 units

compile time jump instruction fully known
------------------------------------------
i = 7 : jnz c -2
if c is nonzero then i = i - 2 , we can simply call f5 ()
otherwise next instruction i = i + 1 , we can simply call f8 ()

(gen 7 (jnz c -2))
fnext expands to f8()



fnext optimisation
-------------------
run command like
 at i = 1 : dec b
  know next instruction follows at i = 2 so just call f2 ()
