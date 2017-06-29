open Parsed

let parse s = Lexing.from_string s |> Parser.fsm Lexer.read

let%test_unit "parse: let x = 7" =
  [%test_result: t]
  (parse "let x = 7")
  ~expect:[Let (Identifier "x", Int 7)]

let%test_unit "parse: type t = A" =
  [%test_result: t]
  (parse "type t = A")
  ~expect:[Type (Identifier "t", [Variant "A"])]

let%test_unit "parse: type t = A | B | C" =
  [%test_result: t]
  (parse "type t = A | B | C")
  ~expect:[Type (Identifier "t", [Variant "A"; Variant "B"; Variant "C"])]
