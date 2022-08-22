(** Factoring Built-In Int Primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)

let break (key, b) =
  let a = 3 in
  let rec brek key a = if modulo key a = 0 then
      (a, quot key a)
    else
      brek key (a + 2)
  in brek key a;;
		
