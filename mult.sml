(* sum : int list -> int 		*)
(* REQUIRES: true		*)
(* ENSURES: sum(L) evaluates to the sum of the integers in L. *)
fun sum [ ] = 0
  | sum (x ::L) = x + (sum L);

(* mult : int list -> int 		*)
(* REQUIRES: true		*)
(* ENSURES: mult(L) evaluates to the product of the integers in L. *)
fun mult [ ] = 1
  | mult (x ::L) = x * (mult L);

(* mult : int list list -> int 	*)
(* REQUIRES: true		*)
(* ENSURES: mult(R) evaluates to the product of all the integers in the lists of R. *)
fun Mult [ ] = 1
  | Mult (r :: R) = (mult r) * (Mult R);

(* mult’ : int list * int -> int 			*)
(* REQUIRES: true				*)
(* ENSURES: mult’(L, a) evaluates to the product of all the integers in the
* lists of R with initial value a. *)
fun mult' ([ ], a) = a
  | mult' (x :: L, a) = mult' (L, x * a);

fun Mult' ([ ], a) = a
  | Mult' (r::R, a) = Mult' (R, mult'(r, a));

(* double : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: double n evaluates to 2 * n.*)
fun double (0 : int) : int = 0
    | double n = 2 + double (n - 1);

fun square (0 : int) : int = 0
  | square (n) = if n < 0 then square(~n) else double(n) - 1 + square(n - 1);

(* divisibleByThree : int -> bool 	*)
(* REQUIRES: true				*)
(* ENSURES: divisibleByThree n evaluates to true if n is a multiple of 3 and to false otherwise *)
fun divisibleByThree (0) = true
  | divisibleByThree (n : int) = 
    if n < 0 then divisibleByThree(~n)
    else if n < 3 then false
    else divisibleByThree(n - 3);

(* evenP : int -> bool 		*)
(* REQUIRES: n >= 0 		*)
(* ENSURES: evenP n evaluates to true iff n is even. *)
fun evenP (0 : int) : bool = true
  | evenP 1 = false
  | evenP n = evenP (n - 2);

fun oddP (0 : int) : bool = false
  | oddP 1 = true
  | oddP n = oddP (n - 2);
