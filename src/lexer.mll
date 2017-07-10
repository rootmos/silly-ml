{
  open Parser
  module L = Lexing
  exception Syntax_error of string
}

let identifier_initial_char = ['a'-'z']
let identifier_subsequent_char = identifier_initial_char | ['A'-'Z' '0'-'9' '_']
let identifier = identifier_initial_char identifier_subsequent_char*

let variant_initial_char = ['A'-'Z']
let variant_subsequent_char = variant_initial_char | ['a'-'z' '0'-'9' '_']
let variant = variant_initial_char variant_subsequent_char*

let wildcard_initial_char = '_'
let wildcard_subsequent_char = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let wildcard = wildcard_initial_char wildcard_subsequent_char*

let int = '-'? ['0'-'9']+
let ws = [' ' '\t' '\n']+

rule read = parse
  | "let" { LET }
  | "type" { TYPE }
  | "of" { OF }
  | "in" { IN }
  | "fun" { FUN }
  | "match" { MATCH }
  | "with" { WITH }
  | "->" { ARROW }
  | ";;" { DOUBLE_SEMICOLON }
  | '=' { EQUAL }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '|' { PIPE }
  | '*' { ASTERISK }
  | ',' { COMMA }
  | '+' { PLUS }
  | '-' { MINUS }
  | ws { read lexbuf }
  | int { INT (int_of_string (L.lexeme lexbuf)) }
  | identifier { IDENTIFIER (L.lexeme lexbuf) }
  | variant { VARIANT (L.lexeme lexbuf) }
  | wildcard { WILDCARD (L.lexeme lexbuf) }
  | eof { EOF }
  | _ { raise (Syntax_error ("Unknown character: " ^ L.lexeme lexbuf)) }

{
  let tokens lexbuf =
    let rec go xs = function
      | EOF -> List.rev (EOF :: xs)
      | x -> go (x :: xs) (read lexbuf) in
    go [] (read lexbuf)
}
