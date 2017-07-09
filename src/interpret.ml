open Core_kernel.Std
open Printf
module L = Lambda

type value =
  V_int of int
| V_unit
| V_tuple of value * value
| V_fun
| V_tag of int * value
[@@deriving sexp]

let compare_value = Pervasives.compare

type error =
  Unbound_value of string
| Match_error
| Unreachable
exception Interpret_exception of error

let format_error = function
 | Unbound_value id -> sprintf "unbound value %s" id
 | Match_error -> "match error"
 | Unreachable -> "interpreter whoopsie"

module Ctx = struct
  type t = {
    bindings: (string * L.value) list
  }

  let empty = { bindings = [] }

  let bind ctx id v = { bindings = (id, v) :: ctx.bindings }
  let lookup ctx id =
    match List.Assoc.find ~equal:(=) ctx.bindings id with
    | Some t -> t
    | None -> raise @@ Interpret_exception (Unbound_value id)
end

let rec pattern_match ctx p v =
  match (p, v) with
  | (_, L.V_ident id) -> Ctx.lookup ctx id |> pattern_match ctx p
  | (L.P_ident id, v') -> Ctx.bind ctx id v'
  | (L.P_tuple (a, b), L.V_tuple (x, y)) ->
      let ctx' = pattern_match ctx a x in
      pattern_match ctx' b y
  | (L.P_int i, L.V_int i') when i = i' -> ctx
  | (L.P_unit, L.V_unit) -> ctx
  | (L.P_wildcard, _) -> ctx
  | (L.P_tag (t, p), L.V_tag (t', v)) -> pattern_match ctx p v
  | _ -> raise @@ Interpret_exception Match_error

let rec reduce ctx = function
  | L.E_value v -> (v, ctx)
  | L.E_let (p, e, body) ->
      let (v, ctx') = reduce ctx e in
      let ctx'' = pattern_match ctx' p v in
      reduce ctx'' body
  | L.E_apply (L.V_ident id, args) ->
      reduce ctx @@ L.E_apply (Ctx.lookup ctx id, args)
  | L.E_apply (L.V_fun (p, body), a :: args) ->
      let (a', ctx') = reduce ctx a in
      let ctx'' = pattern_match ctx' p a' in
      let (body', ctx''') = reduce ctx'' body in
      reduce ctx''' @@ L.E_apply (body', args)
  | L.E_apply (v, []) -> (v, ctx)
  | L.E_apply (_, _ :: _) -> raise @@ Interpret_exception Unreachable
  | L.E_switch (L.V_ident id, cases) ->
      reduce ctx @@ L.E_switch (Ctx.lookup ctx id, cases)
  | L.E_switch (v, (p, body) :: cs) ->
      begin try
        let ctx' = pattern_match ctx p v in
        reduce ctx' body
      with
        | Interpret_exception Match_error ->
            reduce ctx @@ L.E_switch (v, cs)
      end
  | L.E_switch (v, []) ->
      raise @@ Interpret_exception Match_error

let rec reduce_value ctx = function
  | L.V_int i -> V_int i
  | L.V_unit -> V_unit
  | L.V_tuple (a, b) -> V_tuple (reduce_value ctx a, reduce_value ctx b)
  | L.V_ident id -> Ctx.lookup ctx id |> reduce_value ctx
  | L.V_fun _ -> V_fun
  | L.V_tag (t, v) -> V_tag (t, reduce_value ctx v)

let interpret ?ctx:(ctx=Ctx.empty) lambda =
  let (v, ctx') = reduce ctx lambda in
  (reduce_value ctx' v, ctx')

let rec format_value = function
  | V_int i -> string_of_int i
  | V_unit -> "()"
  | V_tuple (a, b) -> "(" ^ format_value a ^ "," ^ format_value b ^ ")"
  | V_fun -> "<fun>"
  | V_tag (t, v) -> string_of_int t ^ "#" ^ format_value v
