(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power 

(* --------------------------------- *)

let rec concatenate list1 list2 = match list1 with
  | e :: l1 -> e :: concatenate l1 list2
  | [] -> match list2 with
      | e2 :: l2 -> e2 :: concatenate list1 l2
      | [] -> [] ;;

let rec reverse list = match list with
  | e :: l -> concatenate (reverse l) [e]
  | [] -> [] ;;

let decomposition word =
  let rec decomposition word n = match n with
    | _ when n = ((String.length word) - 1) -> String.get word n :: []
    | _ -> String.get word n :: decomposition word (n + 1)
  in reverse (decomposition word 0) ;;

let convert n =
  let rec test n a =
    if quot n 2 = 0 then
      (modulo n 2) * a
  else
    match modulo n 2 with
      | 0 -> 0 * a + test (quot n 2) (a * 10)
      | 1 -> 1 * a + test (quot n 2) (a * 10)
      | _ -> invalid_arg "convert: problem with the function"
  in test n 1 ;;

let final n b =
  let rec binary n b = if n = 0 then
      0
    else
      match (modulo n 10) with
	| 0 -> binary (n / 10) (b + 1)
	| 1 -> (power 2 b) + binary ((n - 1) / 10) (b + 1)
	| _ -> invalid_arg "encode: binary conversion: the number must be in base 2"
  in binary n b ;; 

(* --------------------------------- *)

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
*)

let encode str bits =
  if bits < 2 then
    invalid_arg "bits can not be < 2"
      else
  let list = decomposition str in
  let rec encodebis list bits a = match list with
    | e :: l -> final (convert (Char.code e)) a + encodebis l bits (a + bits)
    | [] -> 0 in encodebis list bits 0;;


(* -------------------------------- *)

let rec step1 msg bit =
  let r = modulo msg (power 2 (2 * bit)) and q = quot msg (power 2 (2 * bit)) in
  if q = 0 then
    r :: []
  else
    r :: step1 q bit ;;

let rec base_step msg bit =
  let r = modulo msg (power 2 bit) and q = quot msg (power 2 bit) in
  if q = 0 then
    r :: []
  else
    r :: step1 q bit ;;

let rec step2 list bit = match list with
  | e :: l -> concatenate (base_step e bit) (step2 l bit)
  | [] -> [] ;;

let rec step4 list =
  let l0 = reverse list in
  let rec ending l0 = match l0 with
    | e :: l -> Char.escaped (Char.chr e) ^ ending l
    | [] -> "" in ending l0;;

(*

let z = encode "Bashar" 7 ;;

let b = step1 z 7;;

let c = step2 b 7;;

let d = step4 c;; 

*)


(* -------------------------------- *)

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)

let decode msg bits =
  if bits < 2 then
    invalid_arg "bits can not be < 2"
  else
    step4 (step2 (step1 msg bits) bits) ;;
