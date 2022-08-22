(** Basic arithmetics for ordered euclidian ring. *)

open Scalable;;

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)

let is_zero bA = match bA with
  | [] -> true
  | e :: [] -> invalid_arg "is_zero: this is not a bittaray"
  | e :: l1 -> let rec aux l1 = match l1 with
      | 1 :: l -> false
      | 0 :: [] -> true
      | 0 :: l -> aux l
      | _ -> invalid_arg "is_zero: this is not a bittaray" in aux l1 ;;

(* let rec is_zero_list list = match list with
  | 0 :: [] -> true
  | 1 :: l -> false
  | 0 :: l -> is_zero_list l
  | _ -> false;; 

let rec clear b = if is_zero b then
    [0; 0]
  else
    let rec aux b = match b with
      | 0 :: l -> 0 :: aux l
      | 1 :: l -> 1 :: if is_zero_list l then
	  []
	else
	  aux l
      | _ -> [] in aux b;; *)
  
let rec gcd_b bA bB =
  if (compare_b bA [] = 0 || compare_b bB [] = 0) then
    invalid_arg "bA and bB must be <> 0"
  else
    let rec gcc bA bB = 
  let r = mod_b bA bB in
  if is_zero r then
    if compare_b bB [] = 1 then
      bB
    else
      mult_b bB [1; 1]
  else
    gcc bB r in gcc bA bB;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)


let bezout_b bA bB =
  if (compare_b bA [] = 0 || compare_b bB [] = 0) then
    invalid_arg "bA and bB must be <> 0"
  else
  let u1 = [0; 1] and u2 = [] and v1 = [] and v2 = [0; 1] in
  let rec test bA bB u1 u2 v1 v2 = if (compare_b (mod_b bA bB) []) = 0 then
      (u2, v2, gcd_b bA bB)
    else
      test bB (mod_b bA bB) u2
	(add_b ((mult_b (mult_b (quot_b bA bB) u2) [1; 1])) u1) v2
	(add_b ((mult_b (mult_b (quot_b bA bB) v2) [1; 1])) v1)
  in if compare_b bA bB = 1 then
      test bA bB u1 u2 v1 v2
    else
      test bB bA v1 v2 u1 u2;;

(* TESTS *)

(*

bezout_b (from_int 18) (from_int 22);; 
bezout_b (from_int 22) (from_int 18);;  
bezout_b (from_int 17) (from_int 21);; 
bezout_b (from_int 21) (from_int 17);;

*)

(* OK 

gcd_b (from_int 32) (from_int 6);;

to_int (gcd_b (from_int 32) (from_int 6));;

gcd_b (from_int 18) (from_int 12);;

to_int (gcd_b (from_int 18) (from_int 12));;

gcd_b (from_int (-18)) (from_int (-12));;

to_int (gcd_b (from_int (-18)) (from_int (-12)));;

*)

(*

let p = from_int 9967;;
let q = from_int 9973;;

let n = mult_b p q;;
let phi = mult_b (diff_b p [0; 1]) (diff_b q [0; 1]);;

mod_b phi [0; 0; 1];;

*)



