open Core_kernel.Std
module P = Parsed

type typ =
  T_var of string
| T_tuple of typ * typ
| T_int
[@@deriving sexp]

type pattern =
  P_int of int
| P_ident of string * typ
| P_tuple of pattern * pattern
| P_unit
[@@deriving sexp]

type expression =
  E_int of int
| E_ident of string
| E_apply of expression * expression list * typ
| E_constr of string * expression option
| E_tuple of expression * expression
| E_unit
| E_let of pattern * expression * expression
[@@deriving sexp]

type variant =
  V_constr of (string * typ option)
[@@deriving sexp]

type type_decl = variant list
[@@deriving sexp]

type statement =
    S_let of (pattern * expression)
  | S_type_decl of (string * type_decl)
[@@deriving sexp]

type t = statement list
[@@deriving sexp]


let introduce_types parsed =
  let counter = ref 0 in
  let fresh_typevar () =
    let tv = T_var ("T" ^ string_of_int !counter) in
    counter := !counter + 1;
    tv in
  let pattern = function
    | P.P_int i -> P_int i
    | P.P_unit -> P_unit
    | P.P_ident id -> P_ident (id, fresh_typevar ())
    | _ -> failwith "not implemented" in
  let rec expression = function
    | P.E_int i -> E_int i
    | P.E_ident id -> E_ident id
    | P.E_apply (f, args) ->
        let f' = expression f
        and args' = List.map ~f:expression args in
        E_apply (f', args', fresh_typevar ())
    | _ -> failwith "not implemented" in
  let statement = function
    | P.S_let (p, e) ->
        let p' = pattern p
        and e' = expression e in
        S_let (p', e')
  in List.map ~f:statement parsed
