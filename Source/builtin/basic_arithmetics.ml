(** Basic arithmetics with built-in integers *)

open Builtin ;; 

(* Greater common divisor and smaller common multiple
   implemetations.
*)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integers
    @param b non-zero integer
*)

let rec gcd a b =
  if (b = 0 || a = 0) then
    invalid_arg "a and b must be <> 0"
    else
      let rec gcc a b =
	let r = a - ((a/b) * b) in
	if r <> 0 then
	  gcc b r
	else
	  if b > 0 then
	    b
	  else (-b) in gcc a b;;

(* Extended Euclidean algorithm. Computing Bezout Coefficients. *)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)

let bezout a b =
   if (b = 0 || a = 0) then
    invalid_arg "a and b must be <> 0"
    else
  let u1 = 1 and u2 = 0 and v1 = 0 and v2 = 1 in
  let rec test a b u1 u2 v1 v2 = if modulo a b <> 0 then
      test b (modulo a b) u2 (((-(quot a b)) * u2) + u1) v2 (((-(quot a b)) * v2) + v1)
    else
     (u2, v2, gcd a b) 
  in if a > b then test a b u1 u2 v1 v2 else test b a v1 v2 u1 u2 ;;

