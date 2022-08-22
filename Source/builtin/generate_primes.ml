(** Generating primes *)

open Builtin
open Basic_arithmetics 

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n number of elements in the list of integers.
 *)

let init_eratosthenes n = if n < 2 then
    invalid_arg "init_eratosthenes: n must be >= 2"
  else 
    let rec era n a = if a > n then
	[]
      else
	a :: era n (a + 2)
    in 2 :: era n 3 ;;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)

(* auxiliary function which eliminates all the multiple of e in a list *)

let rec sorting2 list e = match list with
	| a :: l when modulo a e = 0 -> sorting2 l e
	| a :: l -> a :: sorting2 l e
	| [] -> [] ;;


let eratosthenes n =
  let sieve = init_eratosthenes n in
  let rec sorting sieve = match sieve with
    | [] -> []
    | e :: list -> e :: sorting (sorting2 list e) in sorting sieve;;

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)

let write_list li file =
  let ko = open_out file in
  let rec test = function
    | [] -> close_out ko
    | e :: l -> Printf.fprintf ko "%i\n" e ; test l
  in test li ;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)

let write_list_primes n file = write_list (eratosthenes n) file ;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
*)

let input_line_opt in_c =
    try Some (input_line (in_c))
    with End_of_file -> None ;;
	
(** Create a list out of reading a line per line channel.
    @param in_c input channel.
*)
    
let rec create_list in_c = match input_line_opt in_c with
  | Some i -> i :: create_list in_c
  | None -> close_in in_c; [] ;;

(** Load list of primes into OCaml environment.
    @param file path to load from.
*)
  
let read_list_primes file = []

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
*)
  
let rec last_element l = match l with
  | [] -> failwith "Builtin.generate_primes.last_element: Your list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t ;;

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Builtin.generate_primes.last_two: List has \
                          to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t ;;

(* Generating couples of prime numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
*)

let double_primes limit isprime =
  let list = eratosthenes limit in
  let rec double list = match list with
    | e :: l when isprime ((e * 2) + 1) -> (e, (e * 2) + 1) :: double l
    | e :: l -> double l
    | [] -> [] in double list;; 

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
*)

let twin_primes limit isprime =
  let list = eratosthenes limit in
  let rec twin list = match list with
    | e :: l when isprime (e + 2) -> (e, e + 2) :: twin l
    | e :: l -> twin l
    | [] -> [] in twin list ;;
