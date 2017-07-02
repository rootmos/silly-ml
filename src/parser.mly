%token LEFT_PAREN
%token RIGHT_PAREN
%token EQUAL
%token LET
%token OF
%token IN
%token TYPE
%token PIPE
%token FUN
%token ARROW
%token ASTERISK
%token DOUBLE_SEMICOLON
%token COMMA
%token <string> IDENTIFIER
%token <string> VARIANT
%token <int> INT
%token EOF

%start <Parsed.t> program

%{

let rec mk_tuple_expression = function
  | [] -> Parsed.E_unit
  | e :: [] -> e
  | t :: ts -> Parsed.E_tuple (t, mk_tuple_expression ts)

let rec mk_tuple_pattern = function
  | [] -> Parsed.P_unit
  | t :: [] -> t
  | t :: ts -> Parsed.P_tuple (t, mk_tuple_pattern ts)

let rec mk_fun e = function
  | [] -> failwith "nullary functions not supported"
  | p :: [] -> Parsed.E_fun (p, e)
  | p :: ps -> Parsed.E_fun (p, mk_fun e ps)

%}

%%

program:
  | ss = separated_list(DOUBLE_SEMICOLON, statement); EOF { ss }
  ;

statement:
  | LET; i = IDENTIFIER; ps = nonempty_list(pattern); EQUAL; e = expression
    { Parsed.S_let (Parsed.P_ident i, mk_fun e ps) }
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
  | v = VARIANT; e = expression { Parsed.E_constr (v, Some e) }
  | v = VARIANT { Parsed.E_constr (v, None) }
  | f = simple_expression; args = nonempty_list(simple_expression) { Parsed.E_apply (f, args) }
  | LET; i = IDENTIFIER; ps = nonempty_list(pattern); EQUAL; e = expression IN; body = expression
    { Parsed.E_let (Parsed.P_ident i, mk_fun e ps, body) }
  | LET; p = pattern; EQUAL; e = expression; IN body = expression { Parsed.E_let (p, e, body) }
  | FUN; ps = nonempty_list(pattern); ARROW; e = expression { mk_fun e ps }
  | se = simple_expression { se }
  ;

simple_expression:
  | i = INT { Parsed.E_int i }
  | i = IDENTIFIER { Parsed.E_ident i }
  | xs = delimited(LEFT_PAREN, separated_list(COMMA, expression), RIGHT_PAREN) { mk_tuple_expression xs }
  ;

pattern:
  | i = INT { Parsed.P_int i }
  | i = IDENTIFIER { Parsed.P_ident i }
  | xs = delimited(LEFT_PAREN, separated_list(COMMA, pattern), RIGHT_PAREN) { mk_tuple_pattern xs }
  ;
