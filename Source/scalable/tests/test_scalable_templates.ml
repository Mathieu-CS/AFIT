open OUnit2
open Scalable

(* Formatting combination of tuples of tuples. *)
let format_2 out_c (x, y) =
  Printf.sprintf "(%i, %i)" (to_int x) (to_int y)
let format_3 out_c (x, y, z) =
  Printf.sprintf "(%i, %i, %i)" (to_int x) (to_int y) (to_int z)
let format_12 out_c (x, y_2) =
  Printf.sprintf "(%i, %a)" x format_2 y_2
let format_1b2 out_c (x, y_2) =
  Printf.sprintf "(%i, %a)" (to_int x) format_2 y_2
let format_22 out_c (x_2, y_2) =
  Printf.sprintf "(%a, %a)" format_2 x_2 format_2 y_2

(* Formatting lists. *)
let format_L out_c li =
  let rec __format li =
    match li with
    | [] -> ""
    | [h] -> string_of_int (to_int h)
    | h::t ->  (string_of_int (to_int h)) ^ "; " ^ __format t
  in
  "[" ^ (__format li) ^ "]"

let format_L2 out_c li_2 =
  let rec __format li_2 =
    match li_2 with
    | [] -> ""
    | [(h1, h2)] -> Printf.sprintf "(%i, %i)" (to_int h1) (to_int h2)
    | (h1, h2)::t ->
       (Printf.sprintf "(%i, %i)" (to_int h1) (to_int h2)) ^ "; " ^ (__format t)
  in
  "[" ^ (__format li_2) ^ "]"

(* Formatting input-output combinations of tested functions.
   Essentially creating aliases. *)
let format_1_1 = format_2
let format_2_1 out_c (x_2, t) =
  Printf.sprintf "(%a, %i)" format_2 x_2 (to_int t)
let format_2_2 = format_22
let format_2_3 out_c (x_2, t_3) =
  Printf.sprintf "(%a, %a)" format_2 x_2 format_3 t_3
let format_3_1 out_c (x_3, t) =
  Printf.sprintf "(%a, %i)" format_3 x_3 (to_int t)
let format_12_1 out_c (x_12, t) =
  Printf.sprintf "(%a, %i)" format_12 x_12 (to_int t)
let format_1b2_1 out_c (x_12, t) =
  Printf.sprintf "(%a, %i)" format_1b2 x_12 (to_int t)
let format_2_22 out_c (x_2, t_22) =
  Printf.sprintf "(%a, %a)" format_2 x_2 format_22 t_22
let format_1_b out_c (x, b) =
  Printf.sprintf "(%i -> %b)" (to_int x) b
let format_3_b out_c (x_3, b) =
  Printf.sprintf "(%a -> %b)" format_3 x_3 b
let format_1i_L out_c (x, li) =
  Printf.sprintf "%i -> %a" x format_L li
let format_1_L out_c (x, li) =
  Printf.sprintf "%i -> %a" (to_int x) format_L li
let format_1L_b out_c ((x, li), t) =
  Printf.sprintf "(%i, %a) -> %b)" (to_int x) format_L li t
let format_3_L out_c (x_3, li) =
  Printf.sprintf "(%a, %a)" format_3 x_3 format_L li
let format_L_1 out_c (li, t) =
  Printf.sprintf "(%a, %i)" format_L li (to_int t)
let format_L_2 out_c (li, t_2) =
  Printf.sprintf "(%a, %a)" format_L li format_2 t_2
let format_1f_L2 out_c ((x, f), li_2) =
  Printf.sprintf "%i -> %a)" x format_L2 li_2
let format_s1_1 out_c ((s, x), y) =
  Printf.sprintf "(%s, %i) -> %i" s (to_int x) (to_int y)
let format_s1i_1 out_c ((s, x), y) =
  Printf.sprintf "(%s, %i) -> %i" s x (to_int y)
let format_2_s out_c (x_2, s) =
  Printf.sprintf "%a -> %s" format_2 x_2 s
let format_2i_s out_c ((x, y), s) =
  Printf.sprintf "(%i, %i) -> %s" (to_int x) y s
let format_1L1_L out_c ((x, li, y), lit) =
  Printf.sprintf "(%i, %a, %i) -> %a" (to_int x) format_L li (to_int y) format_L lit
let format_212_1 out_c ((x_2, y, z_2), t) =
  Printf.sprintf "(%a, %i, %a) -> %i)" format_2 x_2 (to_int y) format_2 z_2 (to_int t)

(* Aliasing for output printers. *)
let o_printer_1 bA = Printf.sprintf "%i" (to_int bA)
let o_printer_2 = Printf.sprintf "%a" format_2
let o_printer_3 = Printf.sprintf "%a" format_3
let o_printer_22 = Printf.sprintf "%a" format_22
let o_printer_b = Printf.sprintf "%b"
let o_printer_L = Printf.sprintf "%a" format_L
let o_printer_L2 = Printf.sprintf "%a" format_L2
let o_printer_s = Printf.sprintf "%s"

(* To detuple functions. *)
let det_1 f = f
let det_2 f (x, y) = f x y
let det_3 f (x, y, z) = f x y z
let det_12 f (x, y_2) = f x y_2
let det_1f f (x, h) = f x h

(* Templates. *)
let template format_ det o_printer t_name t_function t_list =
  t_name >:::
    (List.map
       (fun (arg, res) ->
         let title = Printf.sprintf "%a" format_ (arg, res) in
         title >::
           (fun test_ctxt -> assert_equal
                               ~msg: t_name
                               ~printer: o_printer
                              res ((det t_function) arg))
       )
       t_list
    )

let template_1_1 = template format_1_1 det_1 o_printer_1
let template_2_1 = template format_2_1 det_2 o_printer_1
let template_2_2 = template format_2_2 det_2 o_printer_2
let template_2_3 = template format_2_3 det_2 o_printer_3
let template_3_1 = template format_3_1 det_3 o_printer_1
let template_1_b = template format_1_b det_1 o_printer_b
let template_3_b = template format_3_b det_3 o_printer_b
let template_1_L = template format_1_L det_1 o_printer_L
let template_1i_L = template format_1i_L det_1 o_printer_L
let template_1L_b = template format_1L_b det_2 o_printer_b
let template_3_L = template format_3_L det_3 o_printer_L
let template_L_1 = template format_L_1 det_1 o_printer_1
let template_L_2 = template format_L_2 det_1 o_printer_2
(* FIXME *)
(* let template_1f_L2 = template format_1f_L2 det_1f o_printer_L2 *)
let template_s1_1 = template format_s1_1 det_2 o_printer_1
let template_s1i_1 = template format_s1i_1 det_2 o_printer_1
let template_2_s = template format_2_s det_2 o_printer_s
let template_2i_s = template format_2i_s det_2 o_printer_s
let template_12_1 = template format_1b2_1 det_12 o_printer_1
let template_2_22 = template format_2_22 det_2 o_printer_22
let template_cple_2 = template format_2_2 det_1 o_printer_2
let template_1L1_L = template format_1L1_L det_3 o_printer_L
let template_212_1 = template format_212_1 det_3 o_printer_1

(* Unit Test Wrapper. *)
let run_test template_ t_name t_function t_list =
  let temp_test = template_ t_name t_function t_list in
  run_test_tt_main temp_test ;;
