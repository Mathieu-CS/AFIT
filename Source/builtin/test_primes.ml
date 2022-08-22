(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power 

(** Deterministic primality test *)

let is_prime n = let rec prime a n =
		   if (a * a) > n then true
		   else
		     if modulo n a <> 0 then
		       prime (a + 1) n
		     else
		       false
		 in prime 2 n ;;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 *)

(* let rec is_pseudo_prime p test_seq = match test_seq with
  | e :: l when e >= p -> if mod_power 3 (p - 1) p = 1 &&
			    mod_power 5 (p - 1) p = 1 &&
			    mod_power 7 (p - 1) p = 1 then true else false
  | _ -> match test_seq with
      | [] -> true
      | e :: l -> if e >= p then true else
	  if mod_power e (p - 1) p = 1
	  then
	    is_pseudo_prime p l
	  else
	    false ;;

*)

let rec is_pseudo_prime p test_seq = match test_seq with
  | e :: l when (mod_power e p p) = (modulo e p) -> is_pseudo_prime p l
  | e :: l -> false
  | [] -> true ;;


