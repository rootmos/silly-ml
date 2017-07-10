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

type value =
  V_int of int
| V_unit
| V_tuple of value * value
| V_ident of string
| V_fun of pattern * expression
| V_tag of int * value
| V_predef of string
[@@deriving sexp]
and expression =
  E_value of value
| E_apply of value * expression list
| E_let of pattern * expression * expression
| E_switch of value * (pattern * expression) list
[@@deriving sexp]

type t = expression
[@@deriving sexp]

type error =
  Unbound_identifier of string
| Unbound_constructor of string
exception Lambda_exception of error

let format_error = function
| Unbound_identifier id -> sprintf "unbound identifier %s" id
| Unbound_constructor c -> sprintf "unbound constructor %s" c

let counter = ref 0

let fresh_identifier () =
  let id = "L" ^ string_of_int !counter in
  counter := !counter + 1; id

let predefined_functions =
  T.predefined_functions
    |> String.Map.keys
    |> String.Set.of_list

module Ctx = struct
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
    match List.Assoc.find ~equal:(=) ctx.identifiers id with
    | Some id' -> V_ident id'
    | None ->
        if String.Set.mem predefined_functions id
        then V_predef id
        else raise @@ Lambda_exception (Unbound_identifier id)

  let bind_type_decl ctx type_decl =
    let cs = List.mapi ~f:(fun i (T.V_constr (c, _)) -> c, i) type_decl in
    { ctx with constructors = cs @ ctx.constructors }

  let lookup_constructor ctx c =
    match List.Assoc.find ~equal:(=) ctx.constructors c with
    | Some t -> t
    | None -> raise @@ Lambda_exception (Unbound_constructor c)

  let reconstruct_constructor type_decl i =
    List.nth_exn type_decl i |> fun (T.V_constr (c, _)) -> c
end

let transform_to_lambda ?(ctx=Ctx.empty) typed =
  let rec pattern ctx = function
    | T.P_int i -> P_int i, ctx
    | T.P_unit -> P_unit, ctx
    | T.P_wildcard _ -> P_wildcard, ctx
    | T.P_ident (id, _) ->
        let id', ctx' = Ctx.new_identifier ctx id in
        P_ident id', ctx'
    | T.P_tuple (a, b) ->
        let a', ctx' = pattern ctx a in
        let b', ctx'' = pattern ctx' b in
        P_tuple (a', b'), ctx''
    | T.P_constr (c, None) -> P_int (Ctx.lookup_constructor ctx c), ctx
    | T.P_constr (c, Some p) ->
        let p', ctx' = pattern ctx p in
        P_tag (Ctx.lookup_constructor ctx' c, p'), ctx' in
  let rec expression ctx = function
    | T.E_int i -> E_value (V_int i)
    | T.E_unit -> E_value (V_unit)
    | T.E_ident id -> E_value (Ctx.lookup_identifier ctx id)
    | T.E_let (p, e, body) ->
        let p', ctx' = pattern ctx p in
        let e' = expression ctx e in
        let body' = expression ctx' body in
        E_let (p', e', body')
    | T.E_tuple (a, b) ->
        let a' = expression ctx a in
        let al = fresh_identifier () in
        let b' = expression ctx b in
        let bl = fresh_identifier () in
        E_let (P_ident al, a',
          E_let (P_ident bl, b', E_value (V_tuple (V_ident al, V_ident bl))))
    | T.E_fun (p, e) ->
        let p', ctx' = pattern ctx p in
        let e' = expression ctx' e in
        E_value (V_fun (p', e'))
    | T.E_apply (f, args, _) ->
        let f' = expression ctx f in
        let fl = fresh_identifier () in
        let args' = List.map ~f:(expression ctx) args in
        E_let (P_ident fl, f', E_apply (V_ident fl, args'))
    | T.E_constr (c, None) ->
        E_value (V_int (Ctx.lookup_constructor ctx c))
    | T.E_constr (c, Some e) ->
        let l = fresh_identifier () in
        E_let (P_ident l, expression ctx e,
          E_value (V_tag (Ctx.lookup_constructor ctx c, V_ident l)))
    | T.E_match (e, cases, _) ->
        let e' = expression ctx e in
        let el = fresh_identifier () in
        let cases' = List.map cases ~f:(fun (p, body) ->
          let p', ctx' = pattern ctx p in
          let body' = expression ctx' body in
          p', body') in
        E_let (P_ident el, e', E_switch (V_ident el, cases')) in
  let rec go ctx = function
    | [] -> E_value V_unit, ctx
    | (T.S_type_decl (_, td)) :: es -> go (Ctx.bind_type_decl ctx td) es
    | (T.S_let (p, e)) :: es ->
        let p', ctx' = pattern ctx p in
        let e' = expression ctx' e in
        let e'', ctx'' = go ctx' es in
        E_let (p', e', e''), ctx''
    | (T.S_expr e) :: [] -> expression ctx e, ctx
    | (T.S_expr e) :: es -> failwith "discarding value" in
  go ctx typed
