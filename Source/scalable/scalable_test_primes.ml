(** Testing for primality *)

open Scalable;;
open Scalable_basic_arithmetics;;
open Scalable_power;;

(** Deterministic primality test *)

let is_prime n = 

  let rec prime a n =
    if compare_b (mult_b a a) n = 1 then
      true
    else
      if (compare_b (mod_b n a) []) <> 0 then
	prime (add_b a [0; 1]) n
      else
	false
  in prime [0; 0; 1] n;;



(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
*)
  
(* let rec is_pseudo_prime p test_seq = let a = (diff_b p [0; 1]) in match test_seq with
  | e :: l when (compare_b e p) >= 0 -> 
    if compare_b (mod_power (from_int 3) (a) p) [0; 1] = 0 &&
       compare_b (mod_power (from_int 5) (a) p) [0; 1] = 0 &&
      compare_b (mod_power (from_int 7) (a) p) [0; 1] = 0
    then true else false
  | _ -> match test_seq with
      | [] -> true
      | e :: l -> if (compare_b e p) >= 0 then
	  true
	else
	  if (compare_b (mod_power e (a) p) [0; 1]) = 0 then
	    is_pseudo_prime p l
	  else
	    false;; *)

let rec is_pseudo_prime p test_seq = match test_seq with
  | a :: l when compare_b (mod_power a p p) (mod_b a p) = 0 -> is_pseudo_prime p l
  | a :: l -> false
  | [] -> true ;; 


(* TESTS *)



(* OK *)

(*

is_pseudo_prime (from_int 11) [from_int 2; from_int 4; from_int 5; from_int 20];;  

is_pseudo_prime (from_int 29) [from_int 30; from_int 41; from_int 52];; 

is_pseudo_prime (from_int 22) [from_int 30; from_int 41; from_int 52];; 

is_pseudo_prime (from_int 27) [from_int 30; from_int 41; from_int 52];;

is_pseudo_prime (from_int 23) [from_int 2; from_int 9; from_int 15; from_int 18];;

is_pseudo_prime (from_int 4) [from_int 2; from_int 9; from_int 15; from_int 18];; 

is_pseudo_prime (from_int 2) [from_int 2; from_int 4; from_int 8; from_int 12];;  

is_pseudo_prime (from_int 15) [from_int 2; from_int 9; from_int 15; from_int 18];; 

*)
