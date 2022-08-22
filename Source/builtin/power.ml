(** Power function implementations for built-in integers *)

open Builtin ;;
open Basic_arithmetics ;; 


(* Naive and fast exponentiation ; already implemented in-class.
 *)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)

let rec pow x n = if n < 0 then
    invalid_arg "pow: n must be > 0"
  else
    if n = 0 then
    1
  else
    x * pow x (n - 1) ;;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)

let rec power x n = if n < 0 then
    invalid_arg "pow: n must be > 0"
  else
  match n with

  | 0 -> 1
  | 1 -> x
  | n when modulo n 2 = 0 -> power (x * x) (n / 2)
  | n -> x * power (x * x) ((n - 1) / 2) ;;

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
*)

(*

let mod_power x n m = let e = 1 and c = 1 in if n = 0 then 1 else
  let rec pawer x n e c m =
    if e = n then
      modulo (c * x) m
    else
      pawer x n (e + 1) (modulo (x * c) m) m
  in pawer x n e c m ;;

*)
(* ------------------------------------------------ *)


let rec lenght list = match list with
  | e :: l -> 1 + lenght l
  | [] -> 0;;

let from_int x = if x = 0 then
    []
      else
  let rec base_2 x =
    if (x/2) <> 0 then
      (modulo x 2) :: base_2 (x/2)
    else
      (modulo x 2) :: [] in base_2 x ;;

let mod_power x n m = if n < 0 then
    invalid_arg "mod_power: exponant must be > 0"
      else
  let list = from_int n and result = 1 in
  let rec total result x list m = match list with
    | 1 :: l -> total (modulo (result * x) m) (modulo (x * x) m) l m
    | 0 :: l -> total result (modulo (x * x) m) l m
    | [] -> result
    | _ -> invalid_arg "mod_power: this is not a binary" in total result x list m;;

(* ------------------------------------------------ *)



(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)

let prime_mod_power x n p = if x = 0 then 0 else 
  if n > p then
      mod_power x (modulo n (p - 1)) p
    else
      mod_power x n p;;

(*
m_power 23 8 9;; 
*)


