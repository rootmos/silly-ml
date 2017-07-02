open Core_kernel.Std
module P = Parsed

type typ =
  T_var of string
| T_tuple of typ * typ
| T_int
| T_unit
| T_fun of typ * typ
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
| E_fun of pattern * expression
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

type constrs = (typ * typ) list
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
    | _ -> failwith "not implemented pattern" in
  let rec expression = function
    | P.E_int i -> E_int i
    | P.E_unit -> E_unit
    | P.E_ident id -> E_ident id
    | P.E_apply (f, args) ->
        let f' = expression f
        and args' = List.map ~f:expression args in
        E_apply (f', args', fresh_typevar ())
    | P.E_fun (p, e) -> E_fun (pattern p, expression e)
    | _ -> failwith "not implemented expression" in
  let statement = function
    | P.S_let (p, e) -> S_let (pattern p, expression e)
    | _ -> failwith "not implemented statement" in
  List.map ~f:statement parsed


type error =
  Unbound_value of string
exception Typed_exception of error

module Ctx = struct
  type t = {
    types: (string * type_decl) list;
    bindings: (string * typ) list
  }

  let empty = { types = []; bindings = [] }

  let bind ctx id t = { ctx with bindings = (id, t) :: ctx.bindings }
  let lookup ctx id =
    match List.Assoc.find ctx.bindings id with
    | Some t -> t
    | None -> raise (Typed_exception (Unbound_value id))
end

let derive_constraints typed =
  let open List in
  let pattern ctx = function
    | P_int _ -> (T_int, ctx)
    | P_unit -> (T_unit, ctx)
    | P_ident (id, t) -> (t, Ctx.bind ctx id t)
    | _ -> failwith "not implemented" in
  let rec expression ctx = function
    | E_int _ -> (T_int, [])
    | E_unit -> (T_unit, [])
    | E_ident id -> (Ctx.lookup ctx id, [])
    | E_apply (f, args, t) ->
        let (ft, cs) = expression ctx f
        and (at, cs') =
          args >>| expression ctx |> fold_right ~init:(t,[]) ~f:(fun (t, cs) (s, cs') -> (T_fun (t, s), cs @ cs'))
        in (t, (ft, at) :: cs @ cs')
    | E_fun (p, body) ->
        let (pt, ctx') = pattern ctx p in
        let (et, cs) = expression ctx' body in
        (T_fun (pt, et), cs)
    | _ -> failwith "not implemented" in
  let statement (ctx, cs) = function
    | S_let (p, e) ->
        let (pt, ctx') = pattern ctx p in
        let (et, cs') = expression ctx' e in
        (ctx', (pt, et) :: cs @ cs')
    | _ -> failwith "not implemented" in
  fold_left ~init:(Ctx.empty, []) ~f:statement typed |> snd
