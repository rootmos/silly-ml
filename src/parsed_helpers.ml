exception Parser_helpers_exception

let parse s =
 try Lexing.from_string s |> Parser.program Lexer.read
 with
 | Parser.Error -> raise Parser_helpers_exception
