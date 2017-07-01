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

let%test_unit "parse: let x = y" =
  [%test_result: t]
  (parse "let x = y")
  ~expect:[S_let (P_ident "x", E_ident "y")]

let%test_unit "parse: let x = A" =
  [%test_result: t]
  (parse "let x = A")
  ~expect:[S_let (P_ident "x", E_const "A")]

let%test_unit "parse: let 7 = x" =
  [%test_result: t]
  (parse "let 7 = x")
  ~expect:[S_let (P_int 7, E_ident "x")]

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
  ~expect:[S_type_decl ("t", [V_nullary "A"])]

let%test_unit "parse: type t = A | B | C" =
  [%test_result: t]
  (parse "type t = A | B | C")
  ~expect:[S_type_decl ("t", [V_nullary "A"; V_nullary "B"; V_nullary "C"])]

let%test_unit "parse: type t = A of int" =
  [%test_result: t]
  (parse "type t = A of int")
  ~expect:[S_type_decl ("t", [V_of ("A", T_ident "int")])]

let%test_unit "parse: type t = A of int * string" =
  [%test_result: t]
  (parse "type t = A of int * string")
  ~expect:[S_type_decl ("t", [V_of ("A", T_tuple ((T_ident "int"), T_ident "string"))])]
