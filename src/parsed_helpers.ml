type error = Parsing | Lexing of string
exception Parser_helpers_exception of error

let parse s =
 try Lexing.from_string s |> Parser.program Lexer.read
 with
 | Parser.Error -> raise @@ Parser_helpers_exception Parsing
 | Lexer.Syntax_error msg -> raise @@ Parser_helpers_exception (Lexing msg)
