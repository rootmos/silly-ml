open Core_kernel.Std
open Printf
module L = Lambda

type value =
  V_int of int
| V_unit
| V_tuple of value * value
| V_fun
| V_tag of int * value
| V_predef
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
  [@@deriving sexp]

  let empty = { bindings = [] }

  let bind ctx id v = { bindings = (id, v) :: ctx.bindings }
  let extend ctx cs = { bindings = cs @ ctx.bindings }
  let bindings ctx = ctx.bindings
  let lookup ctx id =
    match List.Assoc.find ~equal:(=) ctx.bindings id with
    | Some t -> t
    | None -> raise @@ Interpret_exception (Unbound_value id)
end

let rec pattern_match ctx p v =
  match (p, v) with
  | _, L.V_ident id -> Ctx.lookup ctx id |> pattern_match ctx p
  | L.P_ident id, v' -> Ctx.bind ctx id v'
  | L.P_tuple (a, b), L.V_tuple (x, y) ->
      let ctx' = pattern_match ctx a x in
      pattern_match ctx' b y
  | L.P_int i, L.V_int i' when i = i' -> ctx
  | L.P_unit, L.V_unit -> ctx
  | L.P_wildcard, _ -> ctx
  | L.P_tag (t, p), L.V_tag (t', v) when t = t' -> pattern_match ctx p v
  | _ -> raise @@ Interpret_exception Match_error

let rec capture_pattern_match ctx p v =
  match (p, v) with
  | _, L.V_ident id -> Ctx.lookup ctx id |> capture_pattern_match ctx p
  | L.P_ident id, v' -> [id, v']
  | L.P_tuple (a, b), L.V_tuple (x, y) ->
      capture_pattern_match ctx a x @ capture_pattern_match ctx b y
  | L.P_int i, L.V_int i' when i = i' -> []
  | L.P_unit, L.V_unit -> []
  | L.P_wildcard, _ -> []
  | L.P_tag (t, p), L.V_tag (t', v) when t = t' ->
      capture_pattern_match ctx p v
  | _ -> raise @@ Interpret_exception Match_error

let rec reduce ?(l=0) ctx e =
  let rec reduce_value = function
    | L.V_tuple (a, b) -> L.V_tuple (reduce_value a, reduce_value b)
    | L.V_tag (t, v) -> L.V_tag (t, reduce_value v)
    | L.V_ident id -> Ctx.lookup ctx id |> reduce_value
    | L.V_int _ | L.V_unit | L.V_predef _ | L.V_closure _ as v -> v in
  let l = l + 1 in
  if Config.verbose () then begin
    Lambda.sexp_of_t e |> Sexp.to_string_hum |> printf "reduce %d: %s\n" l;
    Ctx.sexp_of_t ctx |> Sexp.to_string_hum |> printf "   ctx: %s\n"
  end;
  match e with
  | L.E_value (L.V_closure (p, e, _)) ->
      L.V_closure (p, e, Ctx.bindings ctx), ctx
  | L.E_value v -> reduce_value v, ctx
  | L.E_let (p, e, body) ->
      reduce ~l ctx @@ L.E_apply (L.E_value (L.V_closure (p, body, [])), [e])
  | L.E_apply (L.E_value (L.V_predef id), args) ->
      let rev_args, ctx' = List.fold_left args ~init:([], ctx)
        ~f:(fun (args, ctx) a ->
          let a', ctx' = reduce ~l ctx a in (a' :: args, ctx')) in
      let args' = List.rev rev_args |> List.map
        ~f:(function
          | L.V_ident id -> Ctx.lookup ctx id
          | v -> v) in
      begin match id, args' with
      | "(+)", L.V_int a :: L.V_int b :: [] ->
          L.V_int (a + b), ctx
      | "(-)", L.V_int a :: L.V_int b :: [] ->
          L.V_int (a - b), ctx
      | "(*)", L.V_int a :: L.V_int b :: [] ->
          L.V_int (a * b), ctx
      | "print_int", L.V_int a :: [] -> print_int a; L.V_unit, ctx
      | "print_newline", L.V_unit :: [] -> print_newline (); L.V_unit, ctx
      | _ -> raise @@ Interpret_exception Unreachable end
  | L.E_apply (L.E_value (L.V_ident id), args) ->
      reduce ~l ctx @@ L.E_apply (L.E_value (Ctx.lookup ctx id), args)
  | L.E_apply (L.E_value (L.V_closure (p, body, cs)), a :: args) ->
      let a', _ = reduce ~l ctx a in
      let cs' = capture_pattern_match ctx p a' in
      let ctx' = (Ctx.extend ctx (cs'@cs)) in
      let body', ctx' = reduce ~l ctx' body in
      reduce ~l ctx' @@ L.E_apply (L.E_value body', args)
  | L.E_apply (L.E_value v, []) ->
      v, ctx
  | L.E_apply (_, _) -> raise @@ Interpret_exception Unreachable
  | L.E_switch (L.V_ident id, cases) ->
      reduce ~l ctx @@ L.E_switch (Ctx.lookup ctx id, cases)
  | L.E_switch (v, (p, body) :: cs) ->
      begin try reduce ~l (pattern_match ctx p v) body with
      | Interpret_exception Match_error ->
        reduce ~l ctx @@ L.E_switch (v, cs)
      end
  | L.E_tuple (x, y) ->
      let x', _ = reduce ~l ctx x
      and y', _ = reduce ~l ctx y in
      L.V_tuple (x', y'), ctx
  | L.E_switch (v, []) ->
      raise @@ Interpret_exception Match_error


let interpret ?(ctx=Ctx.empty) lambda =
  let v, ctx' = reduce ctx lambda in
  if Config.verbose () then begin
    L.sexp_of_value v |> Sexp.to_string_hum |> printf "reduced to: %s\n";
    Ctx.sexp_of_t ctx' |> Sexp.to_string_hum |> printf "   ctx: %s\n"
  end;
  let rec reduce_value ctx = function
    | L.V_int i -> V_int i
    | L.V_unit -> V_unit
    | L.V_tuple (a, b) -> V_tuple (reduce_value ctx a, reduce_value ctx b)
    | L.V_ident id -> Ctx.lookup ctx id |> reduce_value ctx
    | L.V_closure _ -> V_fun
    | L.V_tag (t, v) -> V_tag (t, reduce_value ctx v)
    | L.V_predef _ -> V_predef in
  reduce_value ctx' v, ctx'

let rec format_value = function
  | V_int i -> string_of_int i
  | V_unit -> "()"
  | V_tuple (a, b) -> sprintf "(%s, %s)" (format_value a) (format_value b)
  | V_fun -> "<fun>"
  | V_tag (t, v) -> sprintf "%d#%s" t (format_value v)
  | V_predef -> "<predefined>"
