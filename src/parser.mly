%token LEFT_PAREN
%token RIGHT_PAREN
%token EQUAL
%token LET
%token TYPE
%token PIPE
%token <string> IDENTIFIER
%token <string> VARIANT
%token <int> INT
%token EOF

%start <Parsed.t> fsm
%%

fsm:
  | s = statement; EOF { [s] }
  | EOF { [] }
  ;

statement:
  | LET; i = identifier; EQUAL; e = expression { Parsed.Let (i, e) }
  | TYPE; i = identifier; EQUAL; vs = variants { Parsed.Type (i, vs) }
  ;

variants:
  | vs = separated_nonempty_list(PIPE, variant) { vs }
  ;

variant:
  | v = VARIANT { Parsed.Variant v }
  ;

identifier:
  | i = IDENTIFIER { Parsed.Identifier i }
  ;

expression:
  | i = INT { Parsed.Int i }
  ;
