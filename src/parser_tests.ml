open Parsed
open Parsed_helpers

let%test_unit "parse empty string" =
  [%test_result: t]
  (parse "")
  ~expect:[]

let%test_unit "parse: 7;;" =
  [%test_result: t]
  (parse "7;;")
  ~expect:[S_expr (E_int 7)]

let%test_unit "parse: let x = 7;;" =
  [%test_result: t]
  (parse "let x = 7;;")
  ~expect:[S_let (P_ident "x", E_int 7)]

let%test_unit "parse: let x = (7);;" =
  [%test_result: t]
  (parse "let x = (7);;")
  ~expect:[S_let (P_ident "x", E_int 7)]

let%test_unit "parse: let 7 = x;;" =
  [%test_result: t]
  (parse "let 7 = x;;")
  ~expect:[S_let (P_int 7, E_ident "x")]

let%test_unit "parse: let x = y;;" =
  [%test_result: t]
  (parse "let x = y;;")
  ~expect:[S_let (P_ident "x", E_ident "y")]

let%test_unit "parse: let x = f y;;" =
  [%test_result: t]
  (parse "let x = f y;;")
  ~expect:[S_let (P_ident "x", E_apply (E_ident "f", [E_ident "y"]))]

let%test_unit "parse: let x = f y z;;" =
  [%test_result: t]
  (parse "let x = f y z;;")
  ~expect:[S_let (P_ident "x", E_apply (E_ident "f", [E_ident "y"; E_ident "z"]))]

let%test_unit "parse: let x = (f y) z;;" =
  [%test_result: t]
  (parse "let x = (f y) z;;")
  ~expect:[S_let (P_ident "x", E_apply (E_apply (E_ident "f", [E_ident "y"]), [E_ident "z"]))]

let%test_unit "parse: let x = f (y z);;" =
  [%test_result: t]
  (parse "let x = f (y z);;")
  ~expect:[S_let (P_ident "x", E_apply (E_ident "f", [E_apply (E_ident "y", [E_ident "z"])]))]

let%test_unit "parse: let x = A;;" =
  [%test_result: t]
  (parse "let x = A;;")
  ~expect:[S_let (P_ident "x", E_constr ("A", None))]

let%test_unit "parse: let x = A 7;;" =
  [%test_result: t]
  (parse "let x = A 7;;")
  ~expect:[S_let (P_ident "x", E_constr ("A", Some (E_int 7)))]

let%test_unit "parse: let x = A (1, 2);;" =
  [%test_result: t]
  (parse "let x = A (1, 2);;")
  ~expect:[S_let (P_ident "x", E_constr ("A", Some (E_tuple (E_int 1, E_int 2))))]

let%test_unit "parse: let x = A (1, x, 3);;" =
  [%test_result: t]
  (parse "let x = A (1, x, 3);;")
  ~expect:[S_let (P_ident "x", E_constr ("A", Some (E_tuple (E_int 1, E_tuple (E_ident "x", E_int 3)))))]

let%test_unit "parse: let x = ();;" =
  [%test_result: t]
  (parse "let x = ();;")
  ~expect:[S_let (P_ident "x", E_unit)]

let%test_unit "parse: let x = (1, 2);;" =
  [%test_result: t]
  (parse "let x = (1, 2);;")
  ~expect:[S_let (P_ident "x", E_tuple (E_int 1, E_int 2))]

let%test_unit "parse: let x = ((1, 2), 3);;" =
  [%test_result: t]
  (parse "let x = ((1, 2), 3);;")
  ~expect:[S_let (P_ident "x", E_tuple (E_tuple (E_int 1, E_int 2), E_int 3))]

let%test_unit "parse: let x = (1, (2, 3));;" =
  [%test_result: t]
  (parse "let x = (1, (2, 3));;")
  ~expect:[S_let (P_ident "x", E_tuple (E_int 1, E_tuple (E_int 2, E_int 3)))]

let%test_unit "parse: let x = (1, y, 3);;" =
  [%test_result: t]
  (parse "let x = (1, y, 3);;")
  ~expect:[S_let (P_ident "x", E_tuple (E_int 1, E_tuple (E_ident "y", E_int 3)))]

let%test_unit "parse: let () = x;;" =
  [%test_result: t]
  (parse "let () = x;;")
  ~expect:[S_let (P_unit, E_ident "x")]

let%test_unit "parse: let (1, 2) = x;;" =
  [%test_result: t]
  (parse "let (1, 2) = x;;")
  ~expect:[S_let (P_tuple (P_int 1, P_int 2), E_ident "x")]

let%test_unit "parse: let ((1, 2), 3) = x;;" =
  [%test_result: t]
  (parse "let ((1, 2), 3) = x;;")
  ~expect:[S_let (P_tuple (P_tuple (P_int 1, P_int 2), P_int 3), E_ident "x")]

let%test_unit "parse: let (1, (2, 3)) = x;;" =
  [%test_result: t]
  (parse "let (1, (2, 3)) = x;;")
  ~expect:[S_let (P_tuple (P_int 1, P_tuple (P_int 2, P_int 3)), E_ident "x")]

let%test_unit "parse: let (1, y, 3) = x;;" =
  [%test_result: t]
  (parse "let (1, y, 3) = x;;")
  ~expect:[S_let (P_tuple (P_int 1, P_tuple (P_ident "y", P_int 3)), E_ident "x")]

(*let%test_unit "parse: let y = match x with A i -> u | B s -> v" =*)
  (*[%test_result: t]*)
  (*(parse "let y = match x with A i -> u | B s -> v")*)
  (*~expect:[Let (Identifier "y", Match (Identifier "x",)]*)

let%test_unit "parse: let x = 7;; let y = 8;;" =
  [%test_result: t]
  (parse "let x = 7;; let y = 8;;")
  ~expect:[S_let (P_ident "x", E_int 7); S_let (P_ident "y", E_int 8)]

let%test_unit "parse: type t = A;;" =
  [%test_result: t]
  (parse "type t = A;;")
  ~expect:[S_type_decl ("t", [V_constr ("A", None)])]

let%test_unit "parse: type t = A | B | C;;" =
  [%test_result: t]
  (parse "type t = A | B | C;;")
  ~expect:[S_type_decl ("t", [V_constr ("A", None); V_constr ("B", None); V_constr ("C", None)])]

let%test_unit "parse: type t = A of int;;" =
  [%test_result: t]
  (parse "type t = A of int;;")
  ~expect:[S_type_decl ("t", [V_constr ("A", Some (T_ident "int"))])]

let%test_unit "parse: type t = A of int * string;;" =
  [%test_result: t]
  (parse "type t = A of int * string;;")
  ~expect:[S_type_decl ("t", [V_constr ("A", Some (T_tuple ((T_ident "int"), T_ident "string")))])]

let%test_unit "parse: type t = A of int * string | B;;" =
  [%test_result: t]
  (parse "type t = A of int * string | B;;")
  ~expect:[S_type_decl ("t", [V_constr ("A", Some (T_tuple ((T_ident "int"), T_ident "string"))); V_constr ("B", None)])]

let%test_unit "parse: let x = let y = 7 in z;;" =
  [%test_result: t]
  (parse "let x = let y = 7 in z;;")
  ~expect:[S_let (P_ident "x", E_let (P_ident "y", E_int 7, E_ident "z"))]

let%test_unit "parse: let f = fun x -> y;;" =
  [%test_result: t]
  (parse "let f = fun x -> y;;")
  ~expect:[S_let (P_ident "f", E_fun (P_ident "x", E_ident "y"))]

let%test_unit "parse: let f = fun x y -> z;;" =
  [%test_result: t]
  (parse "let f = fun x y -> z;;")
  ~expect:[S_let (P_ident "f", E_fun (P_ident "x", E_fun (P_ident "y", E_ident "z")))]

let%test_unit "parse: let () = let f x = y in ();;" =
  [%test_result: t]
  (parse "let () = let f x = y in ();;")
  ~expect:[S_let (P_unit, E_let (P_ident "f", E_fun (P_ident "x", E_ident "y"), E_unit))]

let%test_unit "parse: let () = let f x y = z in ();;" =
  [%test_result: t]
  (parse "let () = let f x y = z in ();;")
  ~expect:[S_let (P_unit, E_let (P_ident "f", E_fun (P_ident "x", E_fun (P_ident "y", E_ident "z")), E_unit))]

let%test_unit "parse: let f x = y;;" =
  [%test_result: t]
  (parse "let f x = y;;")
  ~expect:[S_let (P_ident "f", E_fun (P_ident "x", E_ident "y"))]

let%test_unit "parse: let f x y = z;;" =
  [%test_result: t]
  (parse "let f x y = z;;")
  ~expect:[S_let (P_ident "f", E_fun (P_ident "x", E_fun (P_ident "y", E_ident "z")))]

let%test_unit "parse: let A = x;;" =
  [%test_result: t]
  (parse "let A = x;;")
  ~expect:[S_let (P_constr ("A", None), E_ident "x")]

let%test_unit "parse: let (A y) = x;;" =
  [%test_result: t]
  (parse "let (A y) = x;;")
  ~expect:[S_let (P_constr ("A", Some (P_ident "y")), E_ident "x")]

let%test_unit "parse: let () = let A = x;;" =
  [%test_result: t]
  (parse "let () = let A = x in ();;")
  ~expect:[S_let (P_unit, E_let (P_constr ("A", None), E_ident "x", E_unit))]

let%test_unit "parse: let () = let (A y) = x;;" =
  [%test_result: t]
  (parse "let () = let (A y) = x in ();;")
  ~expect:[S_let (P_unit, E_let (P_constr ("A", Some (P_ident "y")), E_ident "x", E_unit))]

let%test_unit "parse: let () = match x with A -> a | B _ -> b | 1 -> c | () -> d | _ -> e;;" =
  [%test_result: t]
  (parse "let () = match x with A -> a | B _ -> b | 1 -> c | () -> d | _ -> e;;")
  ~expect:[S_let (P_unit, E_match (E_ident "x", [
    (P_constr ("A", None), E_ident "a");
    (P_constr ("B", Some (P_wildcard)), E_ident "b");
    (P_int 1, E_ident "c");
    (P_unit, E_ident "d");
    (P_wildcard, E_ident "e")]))]

let%test_unit "parse: let _yo = match x with | () -> y;;" =
  [%test_result: t]
  (parse "let _yo = match x with | () -> y;;")
  ~expect:[S_let (P_wildcard, E_match (E_ident "x", [(P_unit, E_ident "y")]))]

let%test_unit "parse: let x = 7 in 7;;" =
  [%test_result: t]
  (parse "let x = 7 in x;;")
  ~expect:[S_expr (E_let (P_ident "x", E_int 7, E_ident "x"))]

let%test_unit "parse: let _ = f A;;" =
  [%test_result: t]
  (parse "let _ = f A;;")
  ~expect:[S_let (P_wildcard, E_apply (E_ident "f", [E_constr ("A", None)]))]

let%test_unit "parse: f A;;" =
  [%test_result: t]
  (parse "f A;;")
  ~expect:[S_expr (E_apply (E_ident "f", [E_constr ("A", None)]))]

let%test_unit "parse: f A;;" =
  [%test_result: t]
  (parse "f A;;")
  ~expect:[S_expr (E_apply (E_ident "f", [E_constr ("A", None)]))]

let%test_unit "parse: 1 + 2;;" =
  [%test_result: t]
  (parse "1 + 2;;")
  ~expect:[S_expr (E_apply (E_ident "(+)", [E_int 1; E_int 2]))]

let%test_unit "parse: 1 + 2 + 3;;" =
  [%test_result: t]
  (parse "(1 + 2) + 3;;")
  ~expect:[S_expr (E_apply (E_ident "(+)", [E_apply (E_ident "(+)", [E_int 1; E_int 2]); E_int 3]))]
