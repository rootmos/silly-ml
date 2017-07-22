%token LEFT_PAREN
%token RIGHT_PAREN
%token EQUAL
%token LET
%token OF
%token IN
%token TYPE
%token PIPE
%token FUN
%token REC
%token ARROW
%token ASTERISK
%token DOUBLE_SEMICOLON
%token SEMICOLON
%token COMMA
%token MATCH
%token WITH
%token PLUS
%token MINUS
%token <string> WILDCARD
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

let mk_rec_fun i e = function
  | [] -> failwith "nullary recursive functions not supported"
  | p :: [] -> Parsed.E_rec_fun (i, p, e)
  | p :: ps -> Parsed.E_rec_fun (i, p, mk_fun e ps)

%}

%%

program:
  | ss = list(terminated(statement, DOUBLE_SEMICOLON)); EOF { ss }
  ;

statement:
  | LET; REC; i = IDENTIFIER; ps = nonempty_list(simple_pattern);
    EQUAL; e = expression
    { Parsed.S_let (Parsed.P_ident i, mk_rec_fun i e ps) }
  | LET; i = IDENTIFIER; ps = nonempty_list(simple_pattern);
    EQUAL; e = expression { Parsed.S_let (Parsed.P_ident i, mk_fun e ps) }
  | LET; p = simple_pattern; EQUAL; e = expression
    { Parsed.S_let (p, e) }
  | TYPE; i = IDENTIFIER; EQUAL; vs = variants
    { Parsed.S_type_decl (i, vs) }
  | e = expression
    { Parsed.S_expr e }
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
  | a = expression_without_sequence; SEMICOLON; b = expression
    { Parsed.E_let (Parsed.P_unit, a, b) }
  | e = expression_without_sequence { e }
  ;

expression_without_sequence:
  | wo_match = expression_without_match_without_sequence { wo_match }
  | m = match_with { m }
  ;

expression_without_match_without_sequence:
  | f = simple_expression; args = nonempty_list(simple_expression_argument)
    { Parsed.E_apply (f, args) }
  | v = VARIANT; e = expression_without_match_without_sequence
    { Parsed.E_constr (v, Some e) }
  | v = VARIANT { Parsed.E_constr (v, None) }
  | LET; REC; i = IDENTIFIER; ps = nonempty_list(simple_pattern); EQUAL;
    e = expression IN; body = expression_without_match_without_sequence
    { Parsed.E_let (Parsed.P_ident i, mk_rec_fun i e ps, body) }
  | LET; i = IDENTIFIER; ps = nonempty_list(simple_pattern); EQUAL;
    e = expression IN; body = expression_without_match_without_sequence
    { Parsed.E_let (Parsed.P_ident i, mk_fun e ps, body) }
  | LET; p = simple_pattern; EQUAL; e = expression; IN;
    body = expression_without_match_without_sequence
    { Parsed.E_let (p, e, body) }
  | FUN; ps = nonempty_list(simple_pattern); ARROW;
    e = expression_without_match_without_sequence { mk_fun e ps }
  | l = simple_expression; op = infix_operator; r = simple_expression
    { Parsed.E_apply (op, [l; r]) }
  | se = simple_expression { se }
  ;

infix_operator:
  | PLUS { Parsed.E_ident "(+)" }
  | MINUS { Parsed.E_ident "(-)" }
  | ASTERISK { Parsed.E_ident "(*)" }
  ;

simple_expression_argument:
  | v = VARIANT { Parsed.E_constr (v, None) }
  | se = simple_expression { se }
  ;

simple_expression:
  | i = INT { Parsed.E_int i }
  | i = IDENTIFIER { Parsed.E_ident i }
  | xs = delimited(LEFT_PAREN, separated_list(COMMA, expression), RIGHT_PAREN)
    { mk_tuple_expression xs }
  ;

pattern:
  | c = VARIANT; p = simple_pattern { Parsed.P_constr (c, Some p) }
  | p = simple_pattern { p }
  ;

simple_pattern:
  | c = VARIANT { Parsed.P_constr (c, None) }
  | i = INT { Parsed.P_int i }
  | WILDCARD { Parsed.P_wildcard }
  | i = IDENTIFIER { Parsed.P_ident i }
  | xs = delimited(LEFT_PAREN, separated_list(COMMA, pattern), RIGHT_PAREN)
    { mk_tuple_pattern xs }
  ;

match_with:
  | MATCH; x = expression; WITH; PIPE?;
    cs = separated_nonempty_list(PIPE, match_case)
    { Parsed.E_match (x, cs) }
  ;

match_case:
  | p = pattern; ARROW; e = expression_without_match_without_sequence
    { (p, e) }
  ;
