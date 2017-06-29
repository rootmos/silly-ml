{
  open Parser
  module L = Lexing
  exception Syntax_error of string
}

let identifier_initial_char = ['a'-'z' '_' '+' '-' '.']
let identifier_subsequent_char = identifier_initial_char | ['A'-'Z' '0'-'9']
let identifier = identifier_initial_char identifier_subsequent_char*

let int = '-'? ['0'-'9']+
let ws = [' ' '\t' '\n']+

rule read = parse
  | "let" { LET }
  | '=' { EQUAL }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | ws { read lexbuf }
  | int { INT (int_of_string (L.lexeme lexbuf)) }
  | identifier { IDENTIFIER (L.lexeme lexbuf) }
  | eof { EOF }
  | _ { raise (Syntax_error ("Unknown character: " ^ L.lexeme lexbuf)) }

{
  let tokens lexbuf =
    let rec go xs = function
      | EOF -> List.rev (EOF :: xs)
      | x -> go (x :: xs) (read lexbuf) in
    go [] (read lexbuf)
}
