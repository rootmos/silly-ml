open Parsed

let parse s = Lexing.from_string s |> Parser.fsm Lexer.read

let%test_unit "parse empty string" =
  [%test_result: t]
  (parse "")
  ~expect:[]

let%test_unit "parse: let x = 7" =
  [%test_result: t]
  (parse "let x = 7")
  ~expect:[Let (Identifier "x", Int 7)]

let%test_unit "parse: let x = 7;; let y = 8" =
  [%test_result: t]
  (parse "let x = 7;; let y = 8")
  ~expect:[Let (Identifier "x", Int 7); Let (Identifier "y", Int 8)]

let%test_unit "parse: type t = A" =
  [%test_result: t]
  (parse "type t = A")
  ~expect:[TypeDecl (Identifier "t", [Variant "A"])]

let%test_unit "parse: type t = A | B | C" =
  [%test_result: t]
  (parse "type t = A | B | C")
  ~expect:[TypeDecl (Identifier "t", [Variant "A"; Variant "B"; Variant "C"])]

let%test_unit "parse: type t = A of int" =
  [%test_result: t]
  (parse "type t = A of int")
  ~expect:[TypeDecl (Identifier "t", [VariantOf ("A", Type (Identifier "int"))])]

let%test_unit "parse: type t = A of int * string" =
  [%test_result: t]
  (parse "type t = A of int * string")
  ~expect:[TypeDecl (Identifier "t", [VariantOf ("A", Tuple (Type (Identifier "int"), Type (Identifier "string")))])]
