(** Generating prime bitarrays *)

open Scalable;;
open Scalable_basic_arithmetics;;

(* Initializing list of bitarrays for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
*)

let init_eratosthenes n = if (compare_b n [0; 0; 1]) < 0 then
    invalid_arg "init_eratosthenes: n must be greater or equal than 2"
  else
    let rec era n a = if (compare_b a n) = 1 then
	[]
      else
	a :: era n (add_b a [0; 0; 1])
    in [0; 0; 1] :: era n [0; 1; 1];;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)

(* --------------------------------- *)

let rec sorting2 list e = match list with
  | a :: l when (compare_b (mod_b a e) []) = 0 -> sorting2 l e
  | a :: l -> a :: sorting2 l e
  | [] -> [] ;;

(* --------------------------------- *)

let eratosthenes n =
  let sieve = init_eratosthenes n in
  let rec sorting sieve = match sieve with
      | [] -> []
      | e :: list -> e :: sorting (sorting2 list e) in sorting sieve;;

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
  
let write_list li file = ();;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)

let write_list_primes n file = ();;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
*)

let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None;;

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)

let create_list in_c = ();;

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
*)
  
let read_list_primes file = [];;

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get last element of a list.
    @param l list of prime bitarrays.
*)
  
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t;;

(** Get two last elements.
    @param l list of prime bitarrays.
*)

let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t ;;

(* Generating couples of prime bitarrays for specific or fun
   purposes.
*)

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)

let double_primes limit isprime =
  let list = eratosthenes limit in
  let rec double list = match list with
    | e :: l when isprime (add_b (mult_b e [0; 0; 1]) [0; 1])-> (e, (add_b (mult_b e [0; 0; 1]) [0; 1])) :: double l
    | e :: l -> double l
    | [] -> [] in double list;; 

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
*)

let twin_primes limit isprime =
  let list = eratosthenes limit in
  let rec twin list = match list with
    | e :: l when isprime (add_b e [0; 0; 1]) -> (e, add_b e [0; 0; 1]) :: twin l
    | e :: l -> twin l
    | [] -> [] in twin list ;;
