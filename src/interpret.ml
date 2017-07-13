open Core_kernel.Std
open Printf
module L = Lambda
open L

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
    bindings: (string * value) list
  } [@@deriving sexp]

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
  | _, V_ident id -> Ctx.lookup ctx id |> pattern_match ctx p
  | P_ident id, v' -> Ctx.bind ctx id v'
  | P_tuple (a, b), V_tuple (x, y) ->
      let ctx' = pattern_match ctx a x in
      pattern_match ctx' b y
  | P_int i, V_int i' when i = i' -> ctx
  | P_unit, V_unit -> ctx
  | P_wildcard, _ -> ctx
  | P_tag (t, p), V_tag (t', v) when t = t' -> pattern_match ctx p v
  | _ -> raise @@ Interpret_exception Match_error

let rec capture_pattern_match ctx p v =
  match (p, v) with
  | _, V_ident id -> Ctx.lookup ctx id |> capture_pattern_match ctx p
  | P_ident id, v' -> [id, v']
  | P_tuple (a, b), V_tuple (x, y) ->
      capture_pattern_match ctx a x @ capture_pattern_match ctx b y
  | P_int i, V_int i' when i = i' -> []
  | P_unit, V_unit -> []
  | P_wildcard, _ -> []
  | P_tag (t, p), V_tag (t', v) when t = t' ->
      capture_pattern_match ctx p v
  | _ -> raise @@ Interpret_exception Match_error

let rec reduce ?(l=0) ctx e =
  let rec reduce_value = function
    | V_tuple (a, b) -> V_tuple (reduce_value a, reduce_value b)
    | V_tag (t, v) -> V_tag (t, reduce_value v)
    | V_ident id -> Ctx.lookup ctx id |> reduce_value
    | V_int _ | V_unit | V_predef _
    | V_captured_closure _ as v -> v in
  let l = l + 1 in
  if Config.verbose () then begin
    Lambda.sexp_of_t e |> Sexp.to_string_hum |> printf "reduce %d: %s\n" l;
    Ctx.sexp_of_t ctx |> Sexp.to_string_hum |> printf "   ctx: %s\n"
  end;
  match e with
  | E_value v -> reduce_value v, ctx
  | E_let (p, e, body) ->
      reduce ~l ctx @@ E_apply (E_value (
        V_captured_closure {
          cc_p = p;
          cc_body = body;
          cc_captures = []
        }), [e])
  | E_apply (E_value (V_predef id), args) ->
      let rev_args, ctx' = List.fold_left args ~init:([], ctx)
        ~f:(fun (args, ctx) a ->
          let a', ctx' = reduce ~l ctx a in (a' :: args, ctx')) in
      let args' = List.(rev_args |> rev >>| reduce_value) in
      begin match id, args' with
      | "(+)", V_int a :: V_int b :: [] -> V_int (a + b), ctx
      | "(-)", V_int a :: V_int b :: [] -> V_int (a - b), ctx
      | "(*)", V_int a :: V_int b :: [] -> V_int (a * b), ctx
      | "print_int", V_int a :: [] -> print_int a; V_unit, ctx
      | "print_newline", V_unit :: [] -> print_newline (); V_unit, ctx
      | _ -> raise @@ Interpret_exception Unreachable end
  | E_apply (E_value (V_ident id), args) ->
      reduce ~l ctx @@ E_apply (E_value (Ctx.lookup ctx id), args)
  | E_apply (E_value (
    V_captured_closure { cc_p; cc_body; cc_captures }), a :: args) ->
      let a', _ = reduce ~l ctx a in
      let cs = capture_pattern_match ctx cc_p a' in
      let ctx' = Ctx.extend ctx @@ cs@cc_captures in
      let body', _ = reduce ~l ctx' cc_body in
      reduce ~l ctx' @@ E_apply (E_value body', args)
  | E_apply (E_value v, []) -> v, ctx
  | E_apply (_, _) -> raise @@ Interpret_exception Unreachable
  | E_uncaptured_closure { uc_p; uc_body; uc_free } ->
      V_captured_closure {
        cc_p = uc_p;
        cc_body = uc_body;
        cc_captures = List.(uc_free >>| fun id -> id, Ctx.lookup ctx id)
      }, ctx
  | E_switch (V_ident id, cases) ->
      reduce ~l ctx @@ E_switch (Ctx.lookup ctx id, cases)
  | E_switch (v, (p, body) :: cs) ->
      begin try reduce ~l (pattern_match ctx p v) body with
      | Interpret_exception Match_error ->
        reduce ~l ctx @@ E_switch (v, cs)
      end
  | E_tuple (x, y) ->
      let x', _ = reduce ~l ctx x
      and y', _ = reduce ~l ctx y in
      V_tuple (x', y'), ctx
  | E_switch (v, []) ->
      raise @@ Interpret_exception Match_error


module I = struct
  type value =
    V_int of int
  | V_unit
  | V_tuple of value * value
  | V_fun
  | V_tag of int * value
  | V_predef
  [@@deriving sexp]

  let compare_value = Pervasives.compare

  let rec of_lambda ctx = function
    | L.V_int i -> V_int i
    | L.V_unit -> V_unit
    | L.V_tuple (a, b) -> V_tuple (of_lambda ctx a, of_lambda ctx b)
    | L.V_ident id -> Ctx.lookup ctx id |> of_lambda ctx
    | L.V_captured_closure _ -> V_fun
    | L.V_tag (t, v) -> V_tag (t, of_lambda ctx v)
    | L.V_predef _ -> V_predef

  let rec to_string = function
    | V_int i -> string_of_int i
    | V_unit -> "()"
    | V_tuple (a, b) -> sprintf "(%s, %s)" (to_string a) (to_string b)
    | V_fun -> "<fun>"
    | V_tag (t, v) -> sprintf "%d#%s" t (to_string v)
    | V_predef -> "<predefined>"
end

let interpret ?(ctx=Ctx.empty) lambda =
  let v, ctx' = reduce ctx lambda in
  if Config.verbose () then begin
    sexp_of_value v |> Sexp.to_string_hum |> printf "reduced to: %s\n";
    Ctx.sexp_of_t ctx' |> Sexp.to_string_hum |> printf "   ctx: %s\n"
  end;
  I.of_lambda ctx' v, ctx'
