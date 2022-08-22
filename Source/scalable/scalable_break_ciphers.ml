(** Factoring bitarrays into primes *)

open Scalable;;
open Scalable_basic_arithmetics;;

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
*)

let break (key, b) =
  let a = [0; 1; 1] in
  let rec brek key a = if mod_b key a = [] then
      (a, quot_b key a)
    else
      brek key (add_b a [0; 0; 1])
  in brek key a;; 
