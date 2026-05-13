
(* aoc 2016 day2 in ocaml - jump straight to part 2 *)

type square = SquareInt of int | SquareChar of char

let one = SquareInt 1
let two = SquareInt 2
let three = SquareInt 3
let four = SquareInt 4
let five = SquareInt 5
let six = SquareInt 6
let seven = SquareInt 7
let eight = SquareInt 8
let nine = SquareInt 9

let letterA = SquareChar 'a'
let letterB = SquareChar 'b'
let letterC = SquareChar 'c'
let letterD = SquareChar 'd'
let letterE = SquareChar 'e'

let start = seven 

let left x =
  if x = three then two
  else if x = six then five
  else if x = four then three
  else if x = seven then six
  else if x = eight then seven
  else if x = nine then eight
  else if x = letterB then letterA
  else if x =letterC then letterB
  else x

let right x =
  if x = two then three
  else if x = three then four
  else if x = five then six
  else if x = six then seven
  else if x = seven then eight
  else if x = eight then nine
  else if x = letterA then letterB
  else if x =letterB then letterC
  else x

let up x =
  if x = three then one
  else if x = six then two
  else if x = seven then three
  else if x = eight then four
  else if x = letterA then six
  else if x = letterB then seven
  else if x = letterC then eight
  else if x = letterD then letterB
  else x

let down x =
  if x = one then three
  else if x = two then six
  else if x = three then seven
  else if x = four then eight
  else if x = six then letterA
  else if x = seven then letterB
  else if x = eight then letterC
  else if x = letterB then letterD
  else x

(*
  List.nth [1;2;3] 2;;
- : int = 3

# List.map (fun x -> x + 2) [1;2;3];;
- : int list = [3; 4; 5]
 *)

let inputLines = [ "ULL" ; "RRDDD" ; "LURDL" ; "UUUUD" ] 

(* define exception *)
exception BadDirection

(* val foo : char list -> square -> square *)

let rec foo = fun (xs : char list) (y : square) : square ->
  match xs with
  | [] -> y
  | h :: t ->
    if h = 'U' then foo t (up y)
    else if h = 'D' then foo t (down y)
    else if h = 'L' then foo t (left y)
    else if h = 'R' then foo t (right y)
    else raise BadDirection

(* given a string list such as xs = [ "ULL" ; "RRDDD" ; "LURDL" ; "UUUUD" ]
   and an initial starting square y = seven
   produce final square we end up at
 *)
let rec code = fun (xs : string list) (y : square) : square ->
    match xs with
    | [] -> y
    | h :: t -> let cs = String.to_seq h |> List.of_seq
                in let out = foo cs y
                   in code t out


let rec codeAcc2 = fun (xs : string list) (y : square) (acc : square list) : (square list ) ->
    match xs with
    | [] -> acc @ [y]
    | h :: t -> let cs = String.to_seq h |> List.of_seq
                in let out = foo cs y
                   in codeAcc2 t out (acc @ [y])

let rec codeAcc = fun (xs : string list)
                      (y : square)
                      (z : square list)
                      : (square list) -> 
                        List.tl (codeAcc2 xs y [])




(* want to start a five which is on the left corner *)
let result = codeAcc inputLines five []

let example1 = codeAcc [] five []

let puzzleLines = ["RUDULRLLUULRURDDRRUDURULLLDRLRLUDDLUDUDDUDRRDUDULDUUULLRULLRLDDLDLDDRLRRRRUDLLDDUULDRLLUDDRRUURLULRRRDLLURRUUDURUDDURLUDDDLUDDUUDUURUDLRDRDRLRDRLDRUDRUUDLRDDRRURDDLRDDRRURDUDDLULLUDRURURRRLRRUDUULULULRRLDLUDUURRLLRUDLLDRDDLRRRULRUDLULDDLLLULDLRUDLLLLRDDLRDRLDRLLRDRRDLRDULULRLLLDRUDRRRUULRUULDRURLUDRURRDLLDLRDLDDDDRRLUDLRRLUUUURDRDDLRRURURRDUULLRLURLURUDDDRDURDUUDRLRLRRLDDLDLDLDDDUDDULURLDDLLRLRRDULUDDLULRLUDDLDLRULUUUDRLDRUDURLUDDRLLRUULDLRRRRDLLLLURULLRDRRUDLUULRRDLLRLRLUDLDDULLDLLRDLDLL";
                   "LLUUUUUUDUDRLRDRDLDURRRLLRRLRURLLUURRLLUDUDLULUURUUURDLUDLDDLULLRDLRUULDLRDUDURLLDDUDUDULLUDDUULLLUULRRRLULRURRDLRUDUDDURRRDRUURDURLLULLRULLDRUULLURLDRDUUDDDDDDRRLDRLRRRLULDDUURRLLLLDRURLURDRDRDURUDUURRDUDUDRLLUUDDRLUDDDRDLDLRLDRURRDLLRULDRLLURURRLUULLRLRRURDDRDRUUURUURUUUDLLRRLUDRLDLRLURLDLUDDUDDDLDUDRRLDLRURULRLLRDUULURRRULDLLLRLDDDUURRRRDULLRURRLULULDLRRUDUDDLRUURDLDUDDUDRRDLRRRDUDUUUDLLDDDDLURLURRRUUULLLULRRLLLLLLULDUUDLRUDRRDLRDUUDUDLLRLDLLRUURDUUURUUUDDLLUUDLULDURLULULUUUDRUDULLURRULRULLRDLDDU";
                   "RLUUURULLDLRLDUDRDURRDUURLLUDDDUULRRRLRLURDDRUULUDULDUUDDDDUDDDDRUDDLDUUDRUDLRRRLLRDDLLLRLLRUULRUULDDRURRLURRLRLULDDRRRDDURDDRDRDULRUDRUUDULRLLULDLRLLDRULRDDRRDDUDLRLLUDRDRRRLUDULRDLRDDURRUUDDRRUDURRUUUDDRRDUDURLUUDUDUURDDDLURLULLUULULURUDUUDRUDULLUUULURDLDUULLDDLLDULRLRLRDUUURUUDLRLDURUDRLDULLUDLDLLRDUURRDUDURLUUUDLLRRULRLULRLDLLURDURRULRLLRRDUDLLRDRRRRDLUUDRUUUDDLRLUDDDDDDRURRRUUURRDLLRURLDDLLDLRRLLLDRRULRRUDLDRDDRRLULURLLUURURURRRRUUUUURUDURLRLLLULULDLLDLRDRRULUDUDRDRRDRDRRDUDLLLRUDRUDDDULRULRRRDRLRUUUURUDURDUUULLULRUDDULDUUDLDURRD";
                   "ULRULDDLDLULLLRRRLRUDDDDDLLDDUDLRRDULUUDRDLRRURDRRLUULRURUDRRULDLLLUDRUUDULULUDDRUDDDRDURRRDRDUUURLRDULUDRDRLDRUDDLLLDRRULUDLUDLDLLRRUDUULULDLDLLUURDLDDLLUUDURLURLLLDRDLDRRLRULUURRDRULRUUURULRRUDDDDLLDLDDLLRRLRRRRDUUDUDLDRDRRURDLRURULDLRDLLLLRUDRLLRDLRLRDURDRUDURRRLRDRDLLRLUDDDDRLRLLDUURRURLUURUULUDLUURDRRUDDLUDUDDDURRDRUDRLRULDULUUUUUUDDUDRUDUUURUDRRDLUDLUUDUULUDURDLDDDLLURRURUUDUDDRRDRLLULULDRLRURRDDDRDUUURDDDRULUDRDDLDURRLDDDLRRRLDDRDURULDLUDLLLURLURRLRRULDLLDDUDRRULDRRRRLURRUULRRRUDLURDLLDLLDULUUDRRLDLLLDRLRUDLUULDLDRUDUDURDRUDRDDDLRLULLUR";
                   "LRLUUURRLRRRRRUURRLLULRLULLDLUDLUDRDDRLDLRLULLURDURLURDLLRLDUUDDURRRRLDLLRULLRLDLLUUDRLDDLLDRULDRLLRURDLRURRUDLULLRURDLURRURUDULLDRLLUUULUDRURRUUDUDULUUULRLDDULDRDLUDDUDDDLRURULLDLLLRLLUURDLRUDLLLLDLLRLRUUUDDRUUUUDLDLRDDURLDURUULLLUUDLLLLDULRRRLLDLDRRDRLUDRUDURLLUDLRLLUDUDRDDDRDLRDLRULUULDRLUDLRLDUURLRRLUDDDUUDDDUDRLDLDUDLURUULLDDDURUUULRLUDLDURUUDRDRURUDDUURDUUUDLLDLDLDURUURLLLLRURUURURULRULLRUDLRRUUUUUDRRLLRDDUURDRDRDDDUDRLURDRRRUDLLLDURDLUUDLLUDDULUUDLDUUULLDRDLRURUURRDURRDLURRRRLLUUULRDULDDLDUURRDLDLLULRRLLUDLDUDLUUL"]

let result2 = codeAcc puzzleLines five []

let directions = fun (x:square) -> [left x ; right x ; up x ; down x ] 

(*
result;;
- : square list = [SquareInt 5; SquareChar 'd'; SquareChar 'b'; SquareInt 3]

# result2;;
- : square list =
[SquareChar 'a'; SquareInt 6; SquareChar 'b'; SquareInt 3; SquareInt 5]
# 
 *)
