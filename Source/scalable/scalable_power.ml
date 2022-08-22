(** Power function implementations for bitarrays *)


open Scalable;;
open Scalable_basic_arithmetics;; 


(* Naive and fast exponentiation ; already implemented in-class in the
   built-in integer case.
*)

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
*)

let pow x n = if (compare_b x []) = 0 then
    []
  else
    if (compare_b n []) = (-1) then
    invalid_arg "pow: n must be > 0"
  else
    let rec puw x n = 
  if compare_b n [] = 0 then
    [0; 1]
  else
    mult_b x (puw x (diff_b n [0; 1])) in puw x n;; 

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
*)

let rec power x n = if (compare_b x []) = 0 then
    []
  else
    if (compare_b n []) = (-1) then
    invalid_arg "power: n must be > 0"
  else
    let rec pouwer x n =
  match n with
  | n when (compare_b n []) = 0 -> [0; 1]
  | n when (compare_b n [0; 1]) = 0 -> x
  | n when compare_b (mod_b n [0; 0; 1]) [] = 0 -> pouwer (mult_b x x) (quot_b n [0; 0; 1])
  | n -> mult_b x (pouwer (mult_b x x) (quot_b (diff_b n [0; 1]) [0; 0; 1])) in pouwer x n;;

(* Modular expnonentiation ; modulo a given natural (bitarray without
   sign bits).
*)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
*)
  
(* let mod_power x n m =
  let e = [0; 1] and c = [0; 1] in
  if (compare_b n [0; 0]) = 0 then
    [0; 1]
  else
    let rec pawer x n e c m =
    if compare_b e n = 0 then
      mod_b (mult_b c x) m
    else
      pawer x n (add_b e [0; 1]) (mult_b c x) m
   in pawer x n e c m ;; *)

let rec unshift b = match b with
  | e :: f :: [] -> []
  | e :: f :: l -> e :: l
  | x :: [] -> invalid_arg "unshift: this is not a bittaray"
  | [] -> [];;

(* ------------------------------- *)

let mod_power x n m = 

  if (compare_b m [0; 1]) = 0 then
    []
  else
    let r = [0; 1] and
	b = mod_b x m in
    let rec puw b n r m =

      if (compare_b n []) = 0 then
	r
      else

	if compare_b (diff_b n (shift (unshift n) 1)) (* (mod_b n [0; 0; 1]) *) [0; 1] = 0 then

	  let r = mod_b (mult_b r b) m and
	      b = mod_b (mult_b b b) m and
	      n = unshift n (* quot_b n [0; 0; 1] *) in
	  puw b n r m

	else

	  let b = mod_b (mult_b b b) m and
	      n = unshift n (* quot_b n [0; 0; 1] *) in
	  puw b n r m

    in puw b n r m;;
	
	

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
*)
  
let prime_mod_power x n p = 

  if (compare_b x []) = 0 then
    []
  else
    if (compare_b n p) = 1 then
      mod_power x (mod_b n (diff_b p [0; 1])) p
    else
      mod_power x n p;;
;;



(* OK *)

(*

power (from_int (-2)) (from_int 2);;
to_int (power (from_int (-2)) (from_int 2));;

power (from_int (-2)) (from_int 3);;
to_int (power (from_int (-2)) (from_int 3));;   

*)

(* power (from_int 2) (from_int 5);;
to_int (power (from_int 2) (from_int 5));;

power (from_int 3) (from_int 3);;
to_int (power (from_int 3) (from_int 3));; 

power [1; 1] (from_int 12);;
to_int (power [1; 1] (from_int 12));;

power (from_int 3) [0; 1];;
to_int (power (from_int 3) [0; 1]);;

power (from_int 5) [0; 0];;
to_int (power (from_int 5) [0; 0]);; 

power [0; 0] (from_int 2);;
to_int (power [0; 0] (from_int 2));;

power [1; 1] (from_int 11);;
to_int (power [1; 1] (from_int 11));;

*)



(* test *)



(* prime_mod_power (from_int (-1)) (from_int 12) (from_int 7);;
from_int 1;;

prime_mod_power (from_int (-1)) (from_int 11) (from_int 11);;
from_int 10;;

prime_mod_power (from_int (0)) (from_int 2) (from_int 3);;
from_int 0;;

prime_mod_power (from_int (3)) (from_int 1) (from_int 3);;
from_int 0;;

prime_mod_power (from_int (5)) (from_int 0) (from_int 2);;
from_int 1;;


prime_mod_power (from_int (-2)) (from_int 2) (from_int 5);;
from_int 4;;

prime_mod_power (from_int (-1)) (from_int 11) (from_int 11);;
from_int (-1);;

prime_mod_power (from_int (-2)) (from_int 3) (from_int 5);;
from_int 2;;

prime_mod_power (from_int (-1)) (from_int 11) (from_int 11);;
from_int 10;;

prime_mod_power (from_int 2) (from_int 5) (from_int 17);;
from_int 15;;

prime_mod_power (from_int (3)) (from_int 3) (from_int 17);;
from_int 10;; *)
                 
(* TESTS FOR RSA *)
(*
#trace mod_power;;


let x = (from_int 281237);;
let n =  (from_int 99400891);;
let m = (from_int 36199003);;

let b = mod_b x m;;

*)
(* mod_power (from_int 281237) (from_int 99400891) (from_int 36199003);; *)

