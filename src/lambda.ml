open Core_kernel.Std
open Utils
module T = Typed

type pattern =
  P_int of int
| P_ident of int
| P_tuple of pattern * pattern
| P_unit
| P_wildcard
| P_tag of int * pattern
[@@deriving sexp]

type value =
  V_int of int
| V_unit
| V_tuple of value * value
| V_ident of int
| V_tag of int * value
| V_predef of string
| V_captured_closure of captured_closure
  [@@deriving sexp]
and expression =
  E_value of value
| E_apply of expression * expression list
| E_let of pattern * expression * expression
| E_tuple of expression * expression
| E_switch of value * switch_case list
| E_uncaptured_closure of uncaptured_closure
  [@@deriving sexp]
and captured_closure = {
  cc_p: pattern;
  cc_body: expression;
  cc_captures: (int * value) list;
  cc_self: int option;
} [@@deriving sexp]
and uncaptured_closure = {
  uc_p: pattern;
  uc_body: expression;
  uc_free: int list;
  uc_self: int option;
} [@@deriving sexp]
and switch_case = {
  sc_p: pattern;
  sc_body: expression;
  sc_free: int list;
}

let rec pattern_captures = function
  | P_int _ | P_unit | P_wildcard -> []
  | P_ident id -> [id]
  | P_tag (_, v) -> pattern_captures v
  | P_tuple (a, b) -> pattern_captures a @ pattern_captures b

let rec free e =
  let open List in
  let rec free_value = function
    | V_int _ | V_unit | V_predef _ | V_captured_closure _ -> []
    | V_ident id -> [id]
    | V_tag (_, v) -> free_value v
    | V_tuple (a, b) -> free_value a @ free_value b in
  match e with
  | E_value v -> free_value v
  | E_apply (f, args) ->
      free f @ concat (args >>| free)
  | E_tuple (a, b) -> free a @ free b
  | E_let (p, e, body) ->
      free e @ set_minus (free body) (pattern_captures p)
  | E_uncaptured_closure uc -> uc.uc_free
  | E_switch (v, cases) ->
      free_value v @ concat (cases >>| fun c -> c.sc_free )

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
  let id = !counter in
  incr counter; id

let predefined_functions =
  T.predefined_functions
    |> String.Map.keys
    |> String.Set.of_list

module Ctx = struct
  type t = {
    identifiers: (string * int) list;
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
        let b' = expression ctx b in
        E_tuple (a', b')
    | T.E_fun (p, body) ->
        let uc_p, ctx' = pattern ctx p in
        let uc_body = expression ctx' body in
        let uc_free = set_minus (free uc_body) (pattern_captures uc_p)
          |> List.dedup in
        E_uncaptured_closure { uc_p ; uc_body; uc_free; uc_self = None }
    | T.E_rec_fun (id, _, p, body) ->
        let self, ctx' = Ctx.new_identifier ctx id in
        let uc_p, ctx'' = pattern ctx' p in
        let uc_body = expression ctx'' body in
        let uc_free = set_minus (free uc_body)
          (self :: pattern_captures uc_p) |> List.dedup in
        E_uncaptured_closure { uc_p ; uc_body; uc_free; uc_self = Some self }
    | T.E_apply (T.E_ident id, args, _) ->
        let args' = List.map ~f:(expression ctx) args in
        E_apply (E_value (Ctx.lookup_identifier ctx id), args')
    | T.E_apply (f, args, _) ->
        let f' = expression ctx f in
        let fl = fresh_identifier () in
        let args' = List.map ~f:(expression ctx) args in
        E_let (P_ident fl, f', E_apply (E_value (V_ident fl), args'))
    | T.E_constr (c, None) ->
        E_value (V_int (Ctx.lookup_constructor ctx c))
    | T.E_constr (c, Some e) ->
        let l = fresh_identifier () in
        E_let (P_ident l, expression ctx e,
          E_value (V_tag (Ctx.lookup_constructor ctx c, V_ident l)))
    | T.E_match (T.E_ident id, cases, _) ->
        let f (p, body) =
          let sc_p, ctx' = pattern ctx p in
          let sc_body = expression ctx' body in
          let sc_free =
            set_minus (free sc_body) (pattern_captures sc_p) |> List.dedup in
          { sc_p; sc_body; sc_free } in
        E_switch (Ctx.lookup_identifier ctx id, List.map cases ~f)
    | T.E_match (e, cases, _) ->
        let f (p, body) =
          let sc_p, ctx' = pattern ctx p in
          let sc_body = expression ctx' body in
          let sc_free = set_minus (free sc_body) (pattern_captures sc_p) in
          { sc_p; sc_body; sc_free } in

        let e = expression ctx e in
        let el = fresh_identifier () in
        let cases = List.map cases ~f in
        E_let (P_ident el, e, E_switch (V_ident el, cases)) in
  let rec go ctx = function
    | [] -> E_value V_unit, ctx
    | (T.S_type_decl (_, td)) :: es -> go (Ctx.bind_type_decl ctx td) es
    | (T.S_let (p, e)) :: es ->
        let p', ctx' = pattern ctx p in
        let e' = expression ctx' e in
        let e'', ctx'' = go ctx' es in
        E_let (p', e', e''), ctx''
    | (T.S_expr e) :: [] -> expression ctx e, ctx
    | (T.S_expr e) :: es ->
        let e' = expression ctx e in
        let e'', ctx' = go ctx es in
        E_let (P_wildcard, e', e''), ctx' in
  go ctx typed
