%token LEFT_PAREN
%token RIGHT_PAREN
%token EQUAL
%token LET
%token OF
%token TYPE
%token PIPE
%token ASTERISK
%token DOUBLE_SEMICOLON
%token COMMA
%token <string> IDENTIFIER
%token <string> VARIANT
%token <int> INT
%token EOF

%start <Parsed.t> program

%{

let mk_tuple_expression xs =
  let rec go = function
    | [] -> Parsed.E_unit
    | t :: [] -> t
    | t :: ts -> Parsed.E_tuple (t, go ts) in
  go xs

let mk_tuple_pattern xs =
  let rec go = function
    | [] -> Parsed.P_unit
    | t :: [] -> t
    | t :: ts -> Parsed.P_tuple (t, go ts) in
  go xs

%}

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
  | v = VARIANT; OF; t = typ { Parsed.V_constr (v, Some t) }
  | v = VARIANT { Parsed.V_constr (v, None) }
  ;

typ:
  | i = IDENTIFIER; ASTERISK; t = typ { Parsed.T_tuple (Parsed.T_ident i, t) }
  | i = IDENTIFIER { Parsed.T_ident i }
  ;

expression:
  | i = INT { Parsed.E_int i }
  | i = IDENTIFIER { Parsed.E_ident i }
  | v = VARIANT; e = expression { Parsed.E_constr (v, Some e) }
  | v = VARIANT { Parsed.E_constr (v, None) }
  | xs = delimited(LEFT_PAREN, separated_list(COMMA, expression), RIGHT_PAREN) { mk_tuple_expression xs }
  ;

pattern:
  | i = INT { Parsed.P_int i }
  | i = IDENTIFIER { Parsed.P_ident i }
  | xs = delimited(LEFT_PAREN, separated_list(COMMA, pattern), RIGHT_PAREN) { mk_tuple_pattern xs }
  ;
