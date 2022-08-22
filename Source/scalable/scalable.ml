(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
context zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code. A natural bitarray is understood as being a bitarray of
which you've taken out the sign bit, it is just the binary
decomposition of a non-negative integer.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)


let power x n = if n = 0 then
    1
  else
    if n < 0 then
      invalid_arg "power: exponant cannot be < 0"
	else
    let rec pow x n = match n with
      | 1 -> x
      | _ when n mod 2 = 0 -> pow (x * x) (n/ 2)
      | _ -> x * pow (x * x) ((n - 1) / 2) in pow x n;;

let rec lenght list = match list with
  | e :: l -> 1 + lenght l
  | [] -> 0;;

let from_int x = if x = 0 then
    []
      else
  let rec base_2 x =
    if (x/2) <> 0 then
      (x mod 2) :: base_2 (x/2)
    else
      (x mod 2) :: []
	in if x > 0 then
	    0 :: base_2 x
	  else
	    if x < 0 then
	      1 :: base_2 (-x)
	    else
	      [] ;;
	
(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
*)

(* ------------------------------------------ *)

let rec to_base_10 list a = match list with
  | e :: l when e = 1 -> (power 2 a) + to_base_10 l (a + 1)
  | e :: l -> to_base_10 l (a + 1)
  | [] -> 0;;

    
(* ------------------------------------------ *)

let to_int bA = match bA with
  | e :: [] -> invalid_arg "to_int : list is not a bitarray"
  | e :: l when e = 1 -> - to_base_10 l 0
  | e :: l when e = 0 -> to_base_10 l 0
  | [] -> 0 
  | _ -> invalid_arg "to_int: wtf" ;;


(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
*)

let print_b bA = ();;

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)

(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 if it is smaller and 0 in case of equality.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
*)

let rec compare_n nA nB = if nA > nB then
    1
  else
    if nA < nB then
      (-1)
    else
      0 ;;

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)

let (>>!) nA nB = (nA > nB);;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)

let (<<!) nA nB = (nA < nB);;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)

let (>=!) nA nB = (nA >= nB);;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB = (nA <= nB);;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 if it smaller and 0 in case of equality.
    @param bA A bitarray.
    @param bB A bitarray.
*)

(* ------------------------------------- *)

let is_zero bA = match bA with
  | e :: [] -> invalid_arg "is_zero: not a bitarray"
  | e :: l1 -> let rec aux l1 = match l1 with
      | 0 :: l -> aux l
      | 1 :: l -> false
      | [] -> true
      | _ -> invalid_arg "is_zero: something's wrong i can feel it" in aux l1
  | [] -> true;;

let rec is_zero_list list = match list with
  | 0 :: [] -> true
  | 1 :: l -> false
  | 0 :: l -> is_zero_list l
  | _ -> false;;

let rec clear b = if is_zero b then
    []
  else
    let rec aux b = match b with
      | 0 :: l -> 0 :: aux l
      | 1 :: l -> 1 :: if is_zero_list l then
	  []
	else
	  aux l
      | _ -> [] in aux b;;
	
(* ----------------------------------- *)

let compare_b bA bB = match (bA, bB) with
  | ( e1 :: l1, e2 :: l2) -> if (e1 = 1) && (e2 = 0) then (-1)
    else
      if (e1 = 0) && (e2 = 1) then
	1
      else
	compare_n (to_int bA) (to_int bB)
  | (0 :: l1, []) -> 1
  | (1 :: l1, []) -> (-1)
  | ([], 1 :: l2) -> 1
  | ([], 0 :: l2) -> (-1)
  | _ -> 0;;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)

let (>>) bA bB = compare_b bA bB = 1;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)

let (<<) bA bB = compare bA bB = (-1);;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)

let (>>=) bA bB = compare bA bB >= 0;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = compare bA bB <= 0;;

(** Sign of a bitarray.
    @param bA Bitarray.
*)

let sign_b bA = match bA with
  | e :: [] -> invalid_arg "to_int : list is not a bitarray"
  | e :: l -> if e = 0 then
      1
    else
      -1
  | [] -> 0;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)

let abs_b bA = if sign_b bA = (-1) then
    match bA with
      | e :: l -> 0 :: l
      | _ -> invalid_arg "abs_b: wtf"
  else
    if sign_b bA = 0 then
      []
    else
      bA ;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)

(* -------------------------------------- *)

let rec abs_n n = if n >= 0 then
    n
  else
    (-n) ;;

(* -------------------------------------- *)

let _quot_t a = if a >= 0 then
    a/2
  else
    a/2 - 1 ;;

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)

let _mod_t a = (abs_n a) - (((abs_n a) / 2) * 2);;

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)

let _div_t a = (_quot_t a, _mod_t a);;

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)

(* -------------------------------------- *)

let rec concatenate list1 list2 = match list1 with
  | e :: l1 -> e :: concatenate l1 list2
  | [] -> list2;;

let rec reverse list = match list with
  | e :: l -> concatenate (reverse l) [e]
  | [] -> [];;

(* -------------------------------------- *)

let add_n nA nB =
  let a = 0 and
  list1 = reverse nA and
      list2 = reverse nB in
  let rec add a list1 list2 = match (list1, list2) with
    |(e1 :: l1, e2 :: l2) -> if (e1 + e2 + a) > 9 then
	((e1 + e2 + a) - 10) :: add 1 l1 l2
      else
	(e1 + e2 + a) :: add 0 l1 l2
    |(e :: l1, []) -> if (e + a) = 10 then
	0 :: add 1 l1 list2
      else
	(e + a) :: add 0 l1 list2
    |([], e :: l2) -> if (e + a) = 10 then
	0 :: add 1 list1 l2
      else
	(e + a) :: add 0 list1 l2
    | _ -> if a = 1 then
	1 :: []
      else
	[] in reverse (add a list1 list2);;

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)

let diff_n nA nB =
  let a = 0 and
  list1 = reverse nA and
  list2 = reverse nB in
  let rec add a list1 list2 = match (list1, list2) with
    |(e1 :: [], e2 :: []) -> if (e1 - e2 - a) < 0 then
	invalid_arg "diff_n: nA must be greater than nB"
      else
	if (e1 - e2 - a) = 0 then
	  []
	else
	  (e1 - e2 - a) :: []
    |(e1 :: l1, e2 :: l2) -> if (e1 - e2 - a) < 0 then
	(10 + (e1 - e2 - a)) :: add 1 l1 l2
      else
	(e1 - e2 - a) :: add 0 l1 l2
    |(e :: l1, []) -> if (e - a) = -1 then
	9 :: add 1 l1 list2
      else
	(e - a) :: add 0 l1 list2
    |([], e :: l2) -> invalid_arg "diff_n: nA must be greater than nB"
    |([], []) -> []
  in reverse (add a list1 list2);;

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)

(* ------------------------------ *)

let rec same_list list1 list2 = match (list1, list2) with
  | ([], []) -> true
  | (e :: l1, f :: l2) when e = f -> same_list l1 l2
  | _ -> false;;
    

let rec add a b1 b2 = match (b1, b2) with
  | ( e1 :: l1, e2 :: l2) -> if _mod_t (e1 + e2 + a) = 0
      then
	0 :: add (if (e1 + e2 + a) > 1 then 1 else 0) l1 l2
      else
	1 :: add (if (e1 + e2 + a) > 1 then 1 else 0) l1 l2
  | (e1 :: l1, []) -> if _mod_t (e1 + a) = 0 then
	0 :: add (if (e1 + a) > 1 then 1 else 0) l1 b2
      else
	1 :: add (if (e1 + a) > 1 then 1 else 0) l1 b2
  | ([], e2 :: l2) -> if _mod_t (e2 + a) = 0 then
	0 :: add (if (e2 + a) > 1 then 1 else 0) b1 l2
      else
	1 :: add (if (e2 + a) > 1 then 1 else 0) b1 l2
  | ([], []) -> if a <> 0 then
	a :: []
      else
      [] ;;

let rec diff a b1 b2 = match (b1, b2) with
  | (e1 :: l1, e2 :: l2) -> if _mod_t (e1 - e2 - a) = 0
    then
      0 :: diff (if (e1 - e2 - a) < 0 then 1 else 0) l1 l2
    else
      1 :: diff (if (e1 - e2 - a) < 0 then 1 else 0) l1 l2
  | (e1 :: l1, []) -> if _mod_t (e1 - a) = 0
    then
      0 :: diff (if (e1 - a) < 0 then 1 else 0) l1 []
    else
      1 :: diff (if (e1 - a) < 0 then 1 else 0) l1 []
  | ([], 0 :: l2) -> []
  | ([], e2 :: l2) -> invalid_arg "diff: this case does not exist"
  | ([], []) -> [] ;;
      
(* ------------------------------ *)

let add_b bA bB = match (bA, bB) with
    | ([], e2 :: l2) -> bB
    | (e1 :: l1, []) -> bA
    | (e1 :: l1, e2 :: l2) when e1 = 0 && e2 = 0 -> 0 :: add 0 l1 l2 (* A + B *)
    | (e1 :: l1, e2 :: l2) when e1 = 1 && e2 = 1 -> 1 :: add 0 l1 l2 (* - (-A + -B)  *)
    | (e1 :: l1, e2 :: l2) when e1 = 1 && e2 = 0 -> clear (if compare_b (abs_b bA) bB = 1 then (* if A > B *)
      1 :: diff 0 l1 l2 (* - (l1 - l2)  *)
    else
      if compare_b (abs_b bA) bB = (-1) then (* if B > A *)
	0 :: diff 0 l2 l1 (*  (B - A) *)
      else
	[])
    | (e1 :: l1, e2 :: l2) when e2 = 1 && e1 = 0 -> clear (if compare_b bA (abs_b bB) = 1 then (* if A > B *)
      0 :: diff 0 l1 l2 (* A - B *)
    else
      if compare_b bA (abs_b bB) = (-1) then (* if B > A *)
	1 :: diff 0 l2 l1 (* - (B - A) *)
      else
	[])
    | _ -> [];;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)

(* ----------------- *)

let rec opposite_b b = match b with
  | [] -> []
  | 0 :: l -> 1 :: l
  | 1 :: l -> 0 :: l
  | _ -> invalid_arg "opposite_b: this shit ain't a bittaray man";;

(* ----------------- *)

let diff_b bA bB = match (bA, bB) with
  | (e1 :: l1, []) -> bA
  | ([], e2 :: l2) -> opposite_b bB
  | (e1 :: l1, e2 :: l2) when e1 = 0 && e2 = 0 -> clear (if compare_b bA bB = 1 then (* if A > B *)
      0 :: diff 0 l1 l2 (* A - B *)
    else
      if compare_b bA bB = (-1) then (* if B > A *)
	1 :: diff 0 l2 l1 (* - (B - A) *)
      else
	[])
    | (e1 :: l1, e2 :: l2) when e1 = 1 && e2 = 1 -> clear (if compare_b bA bB = 1 then (* if A > B *)
      0 :: diff 0 l2 l1 (* B - A *)
    else
      if compare_b bA bB = (-1) then (* if B > A *)
	1 :: diff 0 l1 l2 (* A - B *)
      else
	[])
    | (e1 :: l1, e2 :: l2) when e1 = 1 && e2 = 0 -> 1 :: add 0 l1 l2 
    | (e1 :: l1, e2 :: l2) when e2 = 1 && e1 = 0 -> 0 :: add 0 l1 l2 
    | _ -> [] ;;

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)

let rec add_the_zeros list b = match b with
  | 0 -> list
  | _ -> 0 :: add_the_zeros list (b - 1);;

let rec shift bA d = match bA with
  | e :: [] -> invalid_arg "shift: arg must be a bittaray"
  | 0 :: l -> 0 :: add_the_zeros l d
  | 1 :: l -> 1 :: add_the_zeros l d
  | [] -> []
  | _ -> invalid_arg "shift: i dont know what to put here :/";;

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)

(* let mult bA bB =
  let x = [] in
  let rec multi bA bB x = if (compare_b [] bA = 0) then
    x
  else
      multi (diff_b bA [0; 1]) bB (add_b bB x)
  in if (compare_b bA bB) < 0 then
      multi bA bB x
    else
      multi bB bA x;;

let mult_b bA bB = match (bA, bB) with
  | (0 :: l1, 0 :: l2) -> mult bA bB
  | (1 :: l1, 0 :: l2) -> opposite_b (mult (0 :: l1) bB) 
  | (0 :: l1, 1 :: l2) -> opposite_b (mult bA (0 :: l2))
  | (1 :: l1, 1 :: l2) -> mult (abs_b bA) (abs_b bB) 
  | _ -> [];;

*)

let rec nom_de_fonction list1 bB n = match list1 with
  | 1 :: l -> add_b (shift bB n)  (nom_de_fonction l bB (n + 1))
  | 0 :: l -> nom_de_fonction l bB (n + 1)
  | [] -> []
  | _ -> invalid_arg "nom_de_fonction";;
  
let rec mult_b bA bB =
  let n = 0 in
  let rec bmult bA bB n = match bA with
    | 1 :: l -> opposite_b (nom_de_fonction l bB n)
    | 0 :: l -> (nom_de_fonction l bB n)
    | [] -> []
    | _ -> invalid_arg "binary_mult " in bmult bA bB n;; 

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)

(*
  
  let rec euclide bA bB =
    if compare_b (abs_b bA) (abs_b bB) = -1 then (* if B > A *)
      []
    else
      let x = [] in
      let rec euc bA bB x = if compare_b (diff_b bA bB) [] < 0 then (* if A <= 0 *)
	  x  (* quotient *)
        else
	  euc (diff_b bA bB) bB (add_b x [0; 1]) (* A - B *) (* B *) (* x + 1 *)
      in euc (abs_b bA) (abs_b bB) x;;

  let rec euclide2 bA bB =
    if compare_b (abs_b bA) bB = -1 then (* if B > A *)
      [1; 1]
    else
      let x = [] in
      let rec euc2 bA bB x = if (compare_b bA []) >= 0 then (* if A >= 0 *)
	  x
	else
	  euc2 (add_b bA bB) bB (diff_b x [0; 1]) (* A + B *) (* bB *) (* x - 1 *)
      in euc2 bA bB x;;

*)
  
(* -------------------------------- *)

  let egypt1 bA bB =
    let n = 0 in reverse ((bB, n) ::
    let rec eg bA bB n = if (compare_b bB bA) < 0 then
	(shift bB 1, n + 1) :: (eg bA (shift bB 1) (n + 1))
      else
	[]
    in eg bA bB n);;

  
  let rec quot_b bA bB =
    if is_zero bB then
      invalid_arg "quot_b: division by zero"
    else
      let list = egypt1 (abs_b bA) (abs_b bB) and tot_b = [] and fin = [] in
      
    let rec eggs list tot_b fin  = match list with
	
      | (b, n) :: l when compare_b (abs_b bA) (add_b b tot_b) >= 0 ->
	eggs l (add_b b tot_b) (add_b (shift [0; 1] n) fin)
	  
      | (b, n) :: l -> eggs l tot_b fin
	
      | [] -> if (sign_b bA) = (sign_b bB) then
	  fin
	else 
	     if compare_b (mult_b (opposite_b fin) bB) bA = 0 then
	       opposite_b fin
	     else
		     diff_b (opposite_b fin) [0; 1]
	
    in eggs list tot_b fin;;
      

 (* -------------------------------- *)

  
(*	  
  
  let quot_b bA bB = if is_zero bA then [] else
      if is_zero bB then
	invalid_arg "quot_b: division by zero"
      else
	match (bA, bB) with
	  | (e1 :: l1, e2 :: l2) when (e1 = 1 && e2 = 0) -> euclide2 bA bB
	  | (e1 :: l1, e2 :: l2) when (e1 = 0 && e2 = 0) -> euclide bA bB
	  | (0 :: l1, 1 :: l2) -> opposite_b (euclide bA (abs_b bB))
	  | (e1 :: l1, e2 :: l2) when e2 = 1 -> opposite_b (euclide bA (opposite_b bB))
	  | _ -> invalid_arg "quot_b: not a bittaray" ;; 

*)

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
*)

  let mod_b bA bB =
    let b = abs_b bB in
    let a = diff_b bA (mult_b (quot_b bA b) b) in
    if compare_b a b = 0 then
      []
    else
      a ;; 

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)

let div_b bA bB = (quot_b bA bB, mod_b bA bB);;


(* TEST *)

(*

diff_b [] [0; 1];;

diff_b [0; 1] [];;

add_b [] [0; 1];;

add_b [0; 1] [];;

mult_b [] [0; 1];;

mult_b [0; 1] [];;

quot_b [] [0; 1];;

*)

(*
quot_b (from_int 10) (from_int 3);;
to_int (quot_b (from_int 10) (from_int 3));;

quot_b (from_int (-10)) (from_int 3);;
to_int (quot_b (from_int (-10)) (from_int 3));;

quot_b (from_int (10)) (from_int 2);;
to_int (quot_b (from_int (10)) (from_int 2));;

quot_b (from_int (-10)) (from_int 2);;
to_int (quot_b (from_int (-10)) (from_int 2));;

quot_b (from_int 0) (from_int 10);;
quot_b (from_int 10) (from_int 0);;
*)

(*

let p = from_int 9967;;
let q = from_int 9973;;

let n = mult_b p q;;
let phi = mult_b (diff_b p [0; 1]) (diff_b q [0; 1]);;

let a = mult_b (quot_b phi [0; 0; 1]) [0; 0; 1];;

*)

