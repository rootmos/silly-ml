open Core_kernel.Std
module L = Lambda

type value =
  V_int of int
| V_unit
| V_tuple of value * value
| V_fun
| V_tag of int * value
[@@deriving sexp]

type error =
  Unbound_value of string
| Matching_error
exception Interpret_exception of error

module Ctx = struct
  type t = {
    bindings: (string * L.value) list
  }

  let empty = { bindings = [] }

  let bind ctx id v = { bindings = (id, v) :: ctx.bindings }
  let lookup ctx id =
    match List.Assoc.find ctx.bindings id with
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
  | _ -> raise @@ Interpret_exception Matching_error

let rec reduce ctx = function
  | L.E_value v -> (v, ctx)
  | L.E_let (p, e, body) ->
      let (v, ctx') = reduce ctx e in
      let ctx'' = pattern_match ctx' p v in
      reduce ctx'' body
  | _ -> failwith "not implemented!"

let rec reduce_value ctx = function
  | L.V_int i -> V_int i
  | L.V_unit -> V_unit
  | L.V_tuple (a, b) -> V_tuple (reduce_value ctx a, reduce_value ctx b)
  | L.V_ident id -> Ctx.lookup ctx id |> reduce_value ctx
  | L.V_fun _ -> V_fun
  | L.V_tag (t, v) -> V_tag (t, reduce_value ctx v)

let interpret lambda =
  let (v, ctx) = reduce Ctx.empty lambda in
  reduce_value ctx v
