open Core_kernel.Std
module T = Typed

type pattern =
  P_int of int
| P_ident of string
| P_tuple of pattern * pattern
| P_unit
| P_wildcard
| P_tag of int * pattern
[@@deriving sexp]

type expression =
  E_int of int
| E_unit
| E_tuple of expression * expression
| E_ident of string
| E_apply of expression * expression list
| E_let of pattern * expression * expression
| E_fun of pattern * expression
| E_tag of int * expression
| E_switch of expression * (pattern * expression) list
[@@deriving sexp]

type t = expression
[@@deriving sexp]

type error =
  Unbound_identifier of string
| Unbound_constructor of string
exception Lambda_exception of error

let transform_to_lambda typed =
  let counter = ref 0 in
  let fresh_identifier () =
    let id = "L" ^ string_of_int !counter in
    counter := !counter + 1; id in
  let module Ctx = struct
    type t = {
      identifiers: (string * string) list;
      constructors: (string * int) list
    }

    let empty = { identifiers = []; constructors = [] }

    let new_identifier ctx id =
      let id' = fresh_identifier () in
      let ctx' = { ctx with identifiers = (id, id') :: ctx.identifiers } in
      (id', ctx')

    let lookup_identifier ctx id =
      match List.Assoc.find ctx.identifiers id with
      | Some id' -> id'
      | None -> raise @@ Lambda_exception (Unbound_identifier id)

    let bind_type_decl ctx type_decl =
      let cs = List.mapi ~f:(fun i (T.V_constr (c, _)) -> (c, i)) type_decl in
      { ctx with constructors = cs @ ctx.constructors }

    let lookup_constructor ctx c =
      match List.Assoc.find ctx.constructors c with
      | Some t -> t
      | None -> raise @@ Lambda_exception (Unbound_constructor c)
  end in
  let rec pattern ctx = function
    | T.P_int i -> (P_int i, ctx)
    | T.P_unit -> (P_unit, ctx)
    | T.P_wildcard _ -> (P_wildcard, ctx)
    | T.P_ident (id, _) ->
        let (id', ctx') = Ctx.new_identifier ctx id in
        (P_ident id', ctx')
    | T.P_tuple (a, b) ->
        let (a', ctx') = pattern ctx a in
        let (b', ctx'') = pattern ctx' b in
        (P_tuple (a', b'), ctx'')
    | T.P_constr (c, None) -> (P_int (Ctx.lookup_constructor ctx c), ctx)
    | T.P_constr (c, Some p) ->
        let (p', ctx') = pattern ctx p in
        (P_tag (Ctx.lookup_constructor ctx' c, p'), ctx') in
  let rec expression ctx = function
    | T.E_int i -> E_int i
    | T.E_unit -> E_unit
    | T.E_ident id -> E_ident (Ctx.lookup_identifier ctx id)
    | T.E_let (p, e, body) ->
        let (p', ctx') = pattern ctx p in
        let e' = expression ctx e in
        let body' = expression ctx' body in
        E_let (p', e', body')
    | T.E_tuple (a, b) ->
        let a' = expression ctx a in
        let b' = expression ctx b in
        E_tuple (a', b')
    | T.E_fun (p, e) ->
        let (p', ctx') = pattern ctx p in
        let e' = expression ctx' e in
        E_fun (p', e')
    | T.E_apply (f, args, _) ->
        let f' = expression ctx f in
        let args' = List.map ~f:(expression ctx) args in
        E_apply (f', args')
    | T.E_constr (c, None) ->
        E_int (Ctx.lookup_constructor ctx c)
    | T.E_constr (c, Some e) ->
        E_tag (Ctx.lookup_constructor ctx c, expression ctx e)
    | T.E_match (e, cases, _) ->
        let e' = expression ctx e in
        let cases' = List.map cases ~f:(fun (p, body) ->
          let (p', ctx') = pattern ctx p in
          let body' = expression ctx' body in
          (p', body')) in
        E_switch (e', cases') in
  let rec go ctx = function
    | [] -> E_unit
    | (T.S_type_decl (_, td)) :: es -> go (Ctx.bind_type_decl ctx td) es
    | (T.S_let (p, e)) :: es ->
        let (p', ctx') = pattern ctx p in
        let e' = expression ctx' e in
        E_let (p', e', go ctx' es) in
  go Ctx.empty typed
