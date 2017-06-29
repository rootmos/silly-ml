%token LEFT_PAREN
%token RIGHT_PAREN
%token EQUAL
%token LET
%token <string> IDENTIFIER
%token <int> INT
%token EOF

%start <Parsed.t> fsm
%%

fsm:
  | LET; i = identifier; EQUAL; e = expression { [Parsed.Let (i, e)] }
  | EOF { [] }
  ;

identifier:
  | i = IDENTIFIER { Parsed.Identifier i }
  ;

expression:
  | i = INT { Parsed.Int i }
  ;
