open Parsed

let parse s = Lexing.from_string s |> Parser.program Lexer.read

let%test_unit "parse empty string" =
  [%test_result: t]
  (parse "")
  ~expect:[]

let%test_unit "parse: let x = 7" =
  [%test_result: t]
  (parse "let x = 7")
  ~expect:[S_let (P_ident "x", E_int 7)]

let%test_unit "parse: let x = (7)" =
  [%test_result: t]
  (parse "let x = (7)")
  ~expect:[S_let (P_ident "x", E_int 7)]

let%test_unit "parse: let 7 = x" =
  [%test_result: t]
  (parse "let 7 = x")
  ~expect:[S_let (P_int 7, E_ident "x")]

let%test_unit "parse: let x = y" =
  [%test_result: t]
  (parse "let x = y")
  ~expect:[S_let (P_ident "x", E_ident "y")]

let%test_unit "parse: let x = f y" =
  [%test_result: t]
  (parse "let x = f y")
  ~expect:[S_let (P_ident "x", E_apply (E_ident "f", [E_ident "y"]))]

let%test_unit "parse: let x = f y z" =
  [%test_result: t]
  (parse "let x = f y z")
  ~expect:[S_let (P_ident "x", E_apply (E_ident "f", [E_ident "y"; E_ident "z"]))]

let%test_unit "parse: let x = (f y) z" =
  [%test_result: t]
  (parse "let x = (f y) z")
  ~expect:[S_let (P_ident "x", E_apply (E_apply (E_ident "f", [E_ident "y"]), [E_ident "z"]))]

let%test_unit "parse: let x = f (y z)" =
  [%test_result: t]
  (parse "let x = f (y z)")
  ~expect:[S_let (P_ident "x", E_apply (E_ident "f", [E_apply (E_ident "y", [E_ident "z"])]))]

let%test_unit "parse: let x = A" =
  [%test_result: t]
  (parse "let x = A")
  ~expect:[S_let (P_ident "x", E_constr ("A", None))]

let%test_unit "parse: let x = A 7" =
  [%test_result: t]
  (parse "let x = A 7")
  ~expect:[S_let (P_ident "x", E_constr ("A", Some (E_int 7)))]

let%test_unit "parse: let x = A (1, 2)" =
  [%test_result: t]
  (parse "let x = A (1, 2)")
  ~expect:[S_let (P_ident "x", E_constr ("A", Some (E_tuple (E_int 1, E_int 2))))]

let%test_unit "parse: let x = A (1, x, 3)" =
  [%test_result: t]
  (parse "let x = A (1, x, 3)")
  ~expect:[S_let (P_ident "x", E_constr ("A", Some (E_tuple (E_int 1, E_tuple (E_ident "x", E_int 3)))))]

let%test_unit "parse: let x = ()" =
  [%test_result: t]
  (parse "let x = ()")
  ~expect:[S_let (P_ident "x", E_unit)]

let%test_unit "parse: let x = (1, 2)" =
  [%test_result: t]
  (parse "let x = (1, 2)")
  ~expect:[S_let (P_ident "x", E_tuple (E_int 1, E_int 2))]

let%test_unit "parse: let x = ((1, 2), 3)" =
  [%test_result: t]
  (parse "let x = ((1, 2), 3)")
  ~expect:[S_let (P_ident "x", E_tuple (E_tuple (E_int 1, E_int 2), E_int 3))]

let%test_unit "parse: let x = (1, (2, 3))" =
  [%test_result: t]
  (parse "let x = (1, (2, 3))")
  ~expect:[S_let (P_ident "x", E_tuple (E_int 1, E_tuple (E_int 2, E_int 3)))]

let%test_unit "parse: let x = (1, y, 3)" =
  [%test_result: t]
  (parse "let x = (1, y, 3)")
  ~expect:[S_let (P_ident "x", E_tuple (E_int 1, E_tuple (E_ident "y", E_int 3)))]

let%test_unit "parse: let () = x" =
  [%test_result: t]
  (parse "let () = x")
  ~expect:[S_let (P_unit, E_ident "x")]

let%test_unit "parse: let (1, 2) = x" =
  [%test_result: t]
  (parse "let (1, 2) = x")
  ~expect:[S_let (P_tuple (P_int 1, P_int 2), E_ident "x")]

let%test_unit "parse: let ((1, 2), 3) = x" =
  [%test_result: t]
  (parse "let ((1, 2), 3) = x")
  ~expect:[S_let (P_tuple (P_tuple (P_int 1, P_int 2), P_int 3), E_ident "x")]

let%test_unit "parse: let (1, (2, 3)) = x" =
  [%test_result: t]
  (parse "let (1, (2, 3)) = x")
  ~expect:[S_let (P_tuple (P_int 1, P_tuple (P_int 2, P_int 3)), E_ident "x")]

let%test_unit "parse: let (1, y, 3) = x" =
  [%test_result: t]
  (parse "let (1, y, 3) = x")
  ~expect:[S_let (P_tuple (P_int 1, P_tuple (P_ident "y", P_int 3)), E_ident "x")]

(*let%test_unit "parse: let y = match x with A i -> u | B s -> v" =*)
  (*[%test_result: t]*)
  (*(parse "let y = match x with A i -> u | B s -> v")*)
  (*~expect:[Let (Identifier "y", Match (Identifier "x",)]*)

let%test_unit "parse: let x = 7;; let y = 8" =
  [%test_result: t]
  (parse "let x = 7;; let y = 8")
  ~expect:[S_let (P_ident "x", E_int 7); S_let (P_ident "y", E_int 8)]

let%test_unit "parse: type t = A" =
  [%test_result: t]
  (parse "type t = A")
  ~expect:[S_type_decl ("t", [V_constr ("A", None)])]

let%test_unit "parse: type t = A | B | C" =
  [%test_result: t]
  (parse "type t = A | B | C")
  ~expect:[S_type_decl ("t", [V_constr ("A", None); V_constr ("B", None); V_constr ("C", None)])]

let%test_unit "parse: type t = A of int" =
  [%test_result: t]
  (parse "type t = A of int")
  ~expect:[S_type_decl ("t", [V_constr ("A", Some (T_ident "int"))])]

let%test_unit "parse: type t = A of int * string" =
  [%test_result: t]
  (parse "type t = A of int * string")
  ~expect:[S_type_decl ("t", [V_constr ("A", Some (T_tuple ((T_ident "int"), T_ident "string")))])]

let%test_unit "parse: type t = A of int * string | B" =
  [%test_result: t]
  (parse "type t = A of int * string | B")
  ~expect:[S_type_decl ("t", [V_constr ("A", Some (T_tuple ((T_ident "int"), T_ident "string"))); V_constr ("B", None)])]

let%test_unit "parse: let x = let y = 7 in z" =
  [%test_result: t]
  (parse "let x = let y = 7 in z")
  ~expect:[S_let (P_ident "x", E_let (P_ident "y", E_int 7, E_ident "z"))]
