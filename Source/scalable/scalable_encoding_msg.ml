(** Encoding Strings *)

open Scalable;;
open Scalable_basic_arithmetics;;
open Scalable_power;;

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
*)

let rec unshift list n = match list with
  | e :: l when n > 1 -> unshift l (n - 1)
  | e :: l -> l
  | _ -> [];;

let rec extract str n = match n with
  | 0 -> String.get str 0 :: []
  | _ -> String.get str n :: extract str (n - 1);;

let convert str =
  let list = extract str ((String.length str) - 1)  in
  let rec con list = match list with
    | e :: l -> (from_int (Char.code e)) :: con l
    | [] -> [] in con list;;

let encode str bits =
  let list = convert str and a = bits in
  let rec enc list bits = match list  with
    | e :: l -> add_b (shift e (bits - a)) (enc l (bits + a))
    | [] -> [] in enc list bits;;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
*)

let rec concatenate list1 list2 = match list1 with
  | e :: l1 -> e :: concatenate l1 list2
  | [] -> match list2 with
      | e2 :: l2 -> e2 :: concatenate list1 l2
      | [] -> [] ;;

let rec reverse list = match list with
  | e :: l -> concatenate (reverse l) [e]
  | [] -> [] ;;

let rec aux list bits = match list with
  | e :: l when bits > 0 -> e :: aux l (bits - 1)
  | _ -> [] ;;

let rec enlist msg bits = match msg with
  | e :: l -> (0 :: (aux msg (bits))) :: (enlist (unshift msg (bits)) bits)
  | _ -> [];;

let rec dl_first_bit msg = match msg with
  | 0 :: l -> l
  | 1 :: l -> l
  | _ -> invalid_arg "dl_first_bit: the argument is not a bittaray";;

let to_char_list msg bits =
  let x = reverse (enlist (dl_first_bit msg) bits) in
  let rec aux x = match x with
    | e :: l -> (Char.chr (to_int e)) :: aux l
    | [] -> [] in aux x;;

let decode msg bits =
  let l0 = to_char_list msg bits in
  let rec ending l0 = match l0 with
    | e :: l -> (Char.escaped e) ^ ending l
    | [] -> "" in ending l0;;


(*
let msg = [0; 0; 1; 0; 0; 1; 1; 1; 1; 0; 0; 0; 0; 1; 1; 0; 0; 0; 1; 0; 1; 1; 1; 1; 0; 0; 1; 1; 1; 1; 0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 0; 0; 1];;

enlist msg 7;;

to_char_list msg 7;;
*)
(*

  char.code
  string.get 
  string.length

*)


