(** Tweaking OCaml built-in euclidean division

The OCaml built-in euclidian divisions operations do not follow the
standard mathematical conventions. We adapt OCaml available
primitives to suit maths conventions.

 **)

(** Sign function
    @param x integer
*)
val sign : int -> int

(** Quotient of two natural numbers.
    This is the quotient in euclidiant division sense.
    @param a dividend
    @param b natural number you divide by.
 *)
val quot : int -> int -> int

(** Modulo of two integers.
    Following Euclidean division. NOT OCAML DEFAULT. Positive integer
    between 0 (included) and modulo (excluded) resulting from euclidian
    division of entry by modulo.

    @param a input integer
    @param b moduli a natural number.
 *)
val modulo : int -> int -> int

(** Division of an integer by a natural number. NOT OCAML DEFAULT.
    Division of an integer by a non-zero integer b is the unique couple
    of integers (q, r) such that a = b*q + r and r is in interval 0, abs b
    where left bound only is not included.
    @param a dividend
    @param b integer you divide by.
*)
val div : int -> int -> (int*int)
