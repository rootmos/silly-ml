open Parsed

let parse s = Lexing.from_string s |> Parser.fsm Lexer.read

let%test_unit "parse: let x = 7" =
  [%test_result: t]
  (parse "let x = 7")
  ~expect:[Let (Identifier "x", Int 7)]
