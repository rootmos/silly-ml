%token LEFT_PAREN
%token RIGHT_PAREN
%token EQUAL
%token LET
%token OF
%token TYPE
%token PIPE
%token ASTERISK
%token DOUBLE_SEMICOLON
%token <string> IDENTIFIER
%token <string> VARIANT
%token <int> INT
%token EOF

%start <Parsed.t> fsm
%%

fsm:
  | ss = separated_list(DOUBLE_SEMICOLON, statement); EOF { ss }
  ;

statement:
  | LET; i = identifier; EQUAL; e = expression { Parsed.Let (i, e) }
  | TYPE; i = identifier; EQUAL; vs = variants { Parsed.TypeDecl (i, vs) }
  ;

variants:
  | vs = separated_nonempty_list(PIPE, variant) { vs }
  ;

variant:
  | v = VARIANT; OF; t = typ { Parsed.VariantOf (v, t) }
  | v = VARIANT { Parsed.Variant v }
  ;

typ:
  | i = identifier; ASTERISK; t = typ { Parsed.Tuple (Parsed.Type i, t) }
  | i = identifier { Parsed.Type i }
  ;

identifier:
  | i = IDENTIFIER { Parsed.Identifier i }
  ;

expression:
  | i = INT { Parsed.Int i }
  ;
