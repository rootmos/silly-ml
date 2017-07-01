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

%start <Parsed.t> program
%%

program:
  | ss = separated_list(DOUBLE_SEMICOLON, statement); EOF { ss }
  ;

statement:
  | LET; p = pattern; EQUAL; e = expression { Parsed.S_let (p, e) }
  | TYPE; i = IDENTIFIER; EQUAL; vs = variants { Parsed.S_type_decl (i, vs) }
  ;

variants:
  | vs = separated_nonempty_list(PIPE, variant) { vs }
  ;

variant:
  | v = VARIANT; OF; t = typ { Parsed.V_of (v, t) }
  | v = VARIANT { Parsed.V_nullary v }
  ;

typ:
  | i = IDENTIFIER; ASTERISK; t = typ { Parsed.T_tuple (Parsed.T_ident i, t) }
  | i = IDENTIFIER { Parsed.T_ident i }
  ;

expression:
  | i = INT { Parsed.E_int i }
  | i = IDENTIFIER { Parsed.E_ident i }
  | v = VARIANT { Parsed.E_const v }
  ;

pattern:
  | i = INT { Parsed.P_int i }
  | i = IDENTIFIER { Parsed.P_ident i }
  ;
