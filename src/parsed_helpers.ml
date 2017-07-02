let parse s = Lexing.from_string s |> Parser.program Lexer.read
