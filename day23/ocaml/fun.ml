


(* declare some mutable variables

   let a = ref 0 ;;
   a is now a reference ? to Int
   to get value of a we dereference using ! operator
   !a
   if (!a != 0) then a is not 0
                else a is zero ...

 *)
let a = ref 0;;
let b = ref 0 ;;
let c = ref 0 ;;
let d = ref 0 ;;

(* some procedures *)
let reset = fun () -> a := 0 ; b := 0; c:= 0; d:= 0 ;;

let rec v0 () = a := 1 ; v1 ()
and v1 () = b := 1 ; v2()
and v2 () = d := 26 ; v3()
and v3 () = if (!c != 0) then v5() else v4()
and v4 () = v9()
and v5 () = c := 7 ; v6()
and v6 () = d := !d + 1 ; v7()
and v7 () = c := !c - 1 ; v8()
and v8 () = if (!c != 0) then v6() else v9() 
and v9 () = c := !a ; v10()
and v10 () = a := !a + 1 ; v11() 
and v11 () = b := !b - 1 ; v12() 
and v12 () = if (!b != 0) then v10() else v13() 
and v13 () = b := !c ; v14() 
and v14 () = d := !d - 1 ; v15() 
and v15 () = if (!d != 0) then v9 () else v16()
and v16 () = c := 18 ; v17 ()
and v17 () = d := 11 ; v18()
and v18 () = a := !a + 1 ; v19() 
and v19 () = d := !d - 1 ; v20()
and v20 () = if (!d != 0) then v18() else v21()
and v21 () = c := !c - 1 ; v22()
and v22 () = if (!c != 0) then v17() else vDone()
and vDone () = ()

let run () =  reset();
              v0();
              Format.printf "--------------------------\n" ;
              Format.printf "a = %d\n" !a ;  
              Format.printf "b = %d\n" !b ; 
              Format.printf "c = %d\n" !c ;
              Format.printf "d = %d\n" !d ;
              Format.printf "--------------------------\n" 


let run2 () =  reset() ;
               c := 1 ;
               v0();
               Format.printf "--------------------------\n" ;
               Format.printf "a = %d\n" !a ;
               Format.printf "b = %d\n" !b;
               Format.printf "c = %d\n" !c;
               Format.printf "d = %d\n" !d;
               Format.printf "--------------------------\n" 



let _ = run();;
let _ = run2();;

(* let partA () = run() ; !a ;; *)
(*  *)
(* let partB () = run2() ; !a ;; *)


