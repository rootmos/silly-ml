open Core_kernel.Std

type expression =
  E_int of int
| E_ident of string
| E_const of string
| E_tuple of expression * expression
| E_unit
[@@deriving sexp]

type pattern =
  P_int of int
| P_ident of string
[@@deriving sexp]

type typ =
  T_ident of string
| T_tuple of typ * typ
[@@deriving sexp]

type variant =
  V_nullary of string
| V_of of (string * typ)
[@@deriving sexp]

type type_decl = variant list
[@@deriving sexp]

type statement =
    S_let of (pattern * expression)
  | S_type_decl of (string * type_decl)
[@@deriving sexp]

type t = statement list
[@@deriving sexp]

let compare_sexpr = Pervasives.compare
