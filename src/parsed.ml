open Core_kernel.Std

type pattern =
  P_int of int
| P_ident of string
| P_tuple of pattern * pattern
| P_constr of string * pattern option
| P_unit
| P_wildcard
[@@deriving sexp]

type expression =
  E_int of int
| E_ident of string
| E_apply of expression * expression list
| E_constr of string * expression option
| E_tuple of expression * expression
| E_unit
| E_let of pattern * expression * expression
| E_fun of pattern * expression
| E_rec_fun of string * pattern * expression
| E_match of expression * (pattern * expression) list
[@@deriving sexp]

type typ =
  T_ident of string
| T_tuple of typ * typ
[@@deriving sexp]

type variant =
  V_constr of (string * typ option)
[@@deriving sexp]

type type_decl = variant list
[@@deriving sexp]

type statement =
  S_let of (pattern * expression)
| S_type_decl of (string * type_decl)
| S_expr of expression
[@@deriving sexp]

type t = statement list
[@@deriving sexp]

let compare_sexpr = Pervasives.compare
