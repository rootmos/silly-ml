open Core_kernel.Std
open Utils
module L = Lambda

type pattern =
  P_int of int
| P_ident of int
| P_tuple of pattern * pattern
| P_unit
| P_wildcard
| P_tag of int * pattern
[@@deriving sexp]

type identifier = int
[@@deriving sexp]

type value =
  V_int of int
| V_unit
| V_tuple of value * value
| V_ident of identifier
| V_tag of int * value
  [@@deriving sexp]
and expression =
  E_value of value
| E_primitive of string * value list
| E_apply of value * value
| E_switch of value * (pattern * expression) list
| E_this_and_then of this_and_then
| E_uncaptured_closure of uncaptured_closure
  [@@deriving sexp]
and captured_closure = {
  cc_p: pattern;
  cc_body: expression;
  cc_captures: (identifier * value) list
} [@@deriving sexp]
and uncaptured_closure = {
  uc_p: pattern;
  uc_body: expression;
  uc_free: identifier list
} [@@deriving sexp]
and this_and_then = {
  this: expression;
  and_then: uncaptured_closure
} [@@deriving sexp]

type t = expression
[@@deriving sexp]

type error =
  Unreachable
| Unsupported_primitive_function of string

let format_error = function
| Unreachable -> "anf whoopsie"
| Unsupported_primitive_function pf ->
    sprintf "unsupported primitive function %s" pf

exception Anf_exception of error

let rec pattern_captures = function
  | P_int _ | P_unit | P_wildcard -> []
  | P_ident id -> [id]
  | P_tag (_, v) -> pattern_captures v
  | P_tuple (a, b) -> pattern_captures a @ pattern_captures b

let transform_to_anf lambda =
  let open List in

  let rec pattern = function
    L.P_int i -> P_int i
  | L.P_ident id -> P_ident id
  | L.P_tuple (a, b) -> P_tuple (pattern a, pattern b)
  | L.P_unit -> P_unit
  | L.P_wildcard -> P_wildcard
  | L.P_tag (i, p) -> P_tag (i, pattern p) in

  let bin_op p =
    let a = L.fresh_identifier ()
    and b = L.fresh_identifier () in
    E_uncaptured_closure {
      uc_p = P_ident a;
      uc_free = [];
      uc_body = E_uncaptured_closure {
        uc_p = P_ident b;
        uc_free = [a];
        uc_body = E_primitive (p, [V_ident a; V_ident b])
      }
    } in

  let unary_op p =
    let a = L.fresh_identifier () in
    E_uncaptured_closure {
      uc_p = P_ident a;
      uc_free = [];
      uc_body = E_primitive (p, [V_ident a])
    } in

  let rec value = function
    L.V_int i -> V_int i
  | L.V_unit -> V_unit
  | L.V_tuple (a, b) -> V_tuple (value a, value b)
  | L.V_ident id -> V_ident id
  | L.V_tag (t, v) -> V_tag (t, value v)
  | L.V_predef f ->
      raise @@ Anf_exception Unreachable
  | L.V_captured_closure { L.cc_p; L.cc_body; L.cc_captures } ->
      raise @@ Anf_exception Unreachable
  and expression = function
  | L.E_value (L.V_predef "(+)") -> bin_op "%plus%"
  | L.E_value (L.V_predef "(*)") -> bin_op "%times%"
  | L.E_value (L.V_predef "(-)") -> bin_op "%minus%"
  | L.E_value (L.V_predef "exit") -> unary_op "%exit%"
  | L.E_value (L.V_predef "print_int") -> unary_op "%print_int%"
  | L.E_value (L.V_predef "print_newline") -> unary_op "%print_newline%"
  | L.E_value (L.V_predef pf) ->
      raise @@ Anf_exception (Unsupported_primitive_function pf)
  | L.E_value v -> E_value (value v)
  | L.E_tuple (a, b) ->
      let a' = L.fresh_identifier ()
      and b' = L.fresh_identifier () in
      E_this_and_then {
        this = expression a;
        and_then = {
          uc_p = P_ident a';
          uc_free = [];
          uc_body = E_this_and_then {
            this = expression b;
            and_then = {
              uc_p = P_ident b';
              uc_free = [a'];
              uc_body = E_value (V_tuple (V_ident a', V_ident b'))
            }
          }
        }
      }
  | L.E_let (p, e, body) ->
      E_this_and_then {
        this = expression e;
        and_then = {
          uc_p = pattern p;
          uc_free = set_minus (L.free body) (L.pattern_captures p);
          uc_body = expression body
        }
      }
  | L.E_uncaptured_closure { L.uc_p; L.uc_body; L.uc_free } ->
      let uc_p = pattern uc_p
      and uc_body = expression uc_body in
      E_uncaptured_closure { uc_p; uc_body; uc_free }
  | L.E_apply (f, args) ->
      let rec go id = function
        | [] -> raise @@ Anf_exception Unreachable
        | (e, id', _) :: [] ->
            E_this_and_then {
              this = e;
              and_then = {
                uc_p = P_ident id';
                uc_free = [id];
                uc_body = E_apply (V_ident id, V_ident id')
              }
            }
        | (e, id', _) :: tail ->
            let ts = tail >>| (fun (_, _, fs) -> fs) |> concat |> dedup in
            E_this_and_then {
              this = e;
              and_then = {
                uc_p = P_ident id';
                uc_free = id :: ts;
                uc_body = E_this_and_then {
                  this = E_apply (V_ident id, V_ident id');
                  and_then =
                    let id'' = L.fresh_identifier () in {
                      uc_p = P_ident id'';
                      uc_free = ts;
                      uc_body = go id'' tail;
                    }
                }
              }
            } in
      let xs = args >>| fun e -> expression e, L.fresh_identifier (), L.free e in
      let id = L.fresh_identifier () in
      E_this_and_then {
        this = expression f;
        and_then = {
          uc_p = P_ident id;
          uc_free = xs >>| (fun (_, _, fs) -> fs) |> concat |> dedup;
          uc_body = go id xs
        }
      }
  | _ -> failwith "not implemented: expression" in

  expression lambda

