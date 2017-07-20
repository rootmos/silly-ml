open Core_kernel.Std
module P = Parsed

type typ =
  T_var of string
| T_ident of string
| T_tuple of typ * typ
| T_int
| T_unit
| T_fun of typ * typ
[@@deriving sexp]

let rec format_typ = function
  | T_int -> "int"
  | T_unit -> "unit"
  | T_ident id -> id
  | T_var id -> id
  | T_tuple (a, b) -> sprintf "(%s, %s)" (format_typ a) (format_typ b)
  | T_fun (a, b) -> sprintf "%s -> %s" (format_typ a) (format_typ b)

type pattern =
  P_int of int
| P_ident of string * typ
| P_tuple of pattern * pattern
| P_unit
| P_wildcard of typ
| P_constr of string * pattern option
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
| E_match of expression * (pattern * expression) list * typ
[@@deriving sexp]

type variant =
  V_constr of (string * typ option)
[@@deriving sexp]

type type_decl = variant list
[@@deriving sexp]

type statement =
  S_let of (pattern * expression)
| S_type_decl of (string * type_decl)
| S_expr of expression
[@@deriving sexp]

type t = statement list
[@@deriving sexp]

type constrs = (typ * typ) list
[@@deriving sexp]

let introduce_types parsed =
  let open List in
  let counter = ref 0 in
  let fresh_typevar () =
    let tv = T_var ("T" ^ string_of_int !counter) in
    counter := !counter + 1;
    tv in
  let rec pattern = function
    | P.P_int i -> P_int i
    | P.P_unit -> P_unit
    | P.P_ident id -> P_ident (id, fresh_typevar ())
    | P.P_tuple (a, b) -> P_tuple (pattern a, pattern b)
    | P.P_wildcard -> P_wildcard (fresh_typevar ())
    | P.P_constr (c, op) -> P_constr (c, Option.map ~f:pattern op) in
  let rec expression = function
    | P.E_int i -> E_int i
    | P.E_unit -> E_unit
    | P.E_ident id -> E_ident id
    | P.E_apply (f, args) ->
        let f' = expression f
        and args' = args >>| expression in
        E_apply (f', args', fresh_typevar ())
    | P.E_fun (p, e) -> E_fun (pattern p, expression e)
    | P.E_tuple (a, b) -> E_tuple (expression a, expression b)
    | P.E_let (p, e, body) -> E_let (pattern p, expression e, expression body)
    | P.E_constr (t, oe) -> E_constr (t, Option.map ~f:expression oe)
    | P.E_match (e, cases) ->
        let cases' = cases >>| fun (p, e) -> (pattern p, expression e) in
        E_match (expression e, cases', fresh_typevar ()) in
  let rec typ = function
    | P.T_ident id when id = "int" -> T_int
    | P.T_ident id when id = "unit" -> T_unit
    | P.T_ident id -> T_ident id
    | P.T_tuple (a, b) -> T_tuple (typ a, typ b) in
  let statement = function
    | P.S_let (p, e) -> S_let (pattern p, expression e)
    | P.S_type_decl (t, decl) ->
        let decl' = decl >>| fun (P.V_constr (c, ot)) -> V_constr (c, Option.map ~f:typ ot) in
        S_type_decl (t, decl')
    | P.S_expr e -> S_expr (expression e) in
  parsed >>| statement


type error =
  Unbound_value of string
| Unbound_constructor of string
| Unbound_type of string
| Unification_failed
| Constructor_arity_mismatch of string
exception Typed_exception of error

let format_error = function
  | Unbound_value id -> sprintf "unbound value %s" id
  | Unbound_constructor c -> sprintf "unbound constructor %s" c
  | Unbound_type t -> sprintf "unbound type %s" t
  | Unification_failed -> "unification failed"
  | Constructor_arity_mismatch c -> sprintf "arity mismatch when applying constructor %s" c

let predefined_functions =
  String.Map.of_alist_exn [
    "(+)", T_fun (T_int, T_fun (T_int, T_int));
    "(-)", T_fun (T_int, T_fun (T_int, T_int));
    "(*)", T_fun (T_int, T_fun (T_int, T_int));
    "print_int", T_fun (T_int, T_unit);
    "print_newline", T_fun (T_unit, T_unit);
    "exit", T_fun (T_int, T_unit);
  ]

module Ctx = struct
  type t = {
    types: (string * type_decl) list;
    bindings: (string * typ) list
  }

  let empty = { types = []; bindings = [] }

  let bind ctx id t = { ctx with bindings = (id, t) :: ctx.bindings }
  let lookup ctx id =
    let b = List.Assoc.find ~equal:(=) ctx.bindings id
    and p = String.Map.find predefined_functions id in
    match Option.merge b p ~f:(fun x y -> x) with
    | Some t -> t
    | None -> raise @@ Typed_exception (Unbound_value id)

  let bind_type ctx t decl = {
    ctx with types = (t, decl) :: ctx.types
  }

  let lookup_constr ctx c =
    let f = function
      | V_constr (c', ot) when c = c' -> Some ot
      | _ -> None in
    let g (t, decl) =
      Option.(List.find_map ~f decl >>| fun ot -> (T_ident t, ot)) in
    match List.find_map ~f:g ctx.types with
    | Some x -> x
    | None -> raise @@ Typed_exception (Unbound_constructor c)

  let lookup_type ctx id =
    match List.Assoc.find ~equal:(=) ctx.types id with
    | Some x -> x
    | None -> raise @@ Typed_exception (Unbound_type id)
end

let derive_constraints ?(ctx=Ctx.empty) typed =
  let open List in
  let rec pattern ctx = function
    | P_int _ -> T_int, ctx, []
    | P_unit -> T_unit, ctx, []
    | P_ident (id, t) -> t, Ctx.bind ctx id t, []
    | P_wildcard t -> t, ctx, []
    | P_tuple (a, b) ->
        let at, ctx', cs = pattern ctx a in
        let bt, ctx'', cs' = pattern ctx' b in
        T_tuple (at, bt), ctx'', cs @ cs'
    | P_constr (c, op) ->
        let t, ot = Ctx.lookup_constr ctx c in
        match op, ot with
        | Some p, Some t' ->
            let t'', ctx', cs = pattern ctx p in
            t, ctx', (t', t'') :: cs
        | None, None -> t, ctx, []
        | _ -> raise @@ Typed_exception (Constructor_arity_mismatch c) in
  let rec expression ctx = function
    | E_int _ -> T_int, []
    | E_unit -> T_unit, []
    | E_ident id -> Ctx.lookup ctx id, []
    | E_apply (f, args, t) ->
        let ft, cs = expression ctx f
        and at, cs' =
          args >>| expression ctx
            |> fold_right ~init:(t,[]) ~f:(fun (t, cs) (s, cs') -> T_fun (t, s), cs @ cs')
        in t, (ft, at) :: cs @ cs'
    | E_fun (p, body) ->
        let pt, ctx', cs = pattern ctx p in
        let et, cs' = expression ctx' body in
        T_fun (pt, et), cs @ cs'
    | E_tuple (a, b) ->
        let at, cs = expression ctx a in
        let bt, cs' = expression ctx b in
        T_tuple (at, bt), cs @ cs'
    | E_let (p, e, body) ->
        let pt, ctx', cs = pattern ctx p in
        let et, cs' = expression ctx e
        and bt, cs'' = expression ctx' body in
        et, (pt, et) :: cs @ cs' @ cs''
    | E_match (e, cases, t) ->
        let et, cs = expression ctx e in
        let cs' = fold_left ~init:[] ~f:(fun acc (p, body) ->
          let pt, ctx', xs = pattern ctx p in
          let bodyt, ys = expression ctx' body in
          (et, pt) :: (bodyt, t) :: xs @ ys @ acc) cases in
        t, cs @ cs'
    | E_constr (c, oe) ->
        let t, ot = Ctx.lookup_constr ctx c in
        let cs = match oe, ot with
        | Some e, Some t -> let t', cs' = expression ctx e in (t, t') :: cs'
        | None, None -> []
        | _ -> raise @@ Typed_exception (Constructor_arity_mismatch c) in
        t, cs in
  let statement (ctx, cs, _) = function
    | S_let (p, e) ->
        let pt, ctx', cs' = pattern ctx p in
        let et, cs'' = expression ctx' e in
        ctx', (pt, et) :: cs @ cs' @ cs'', None
    | S_type_decl (t, decl) ->
        let ctx' = Ctx.bind_type ctx t decl in
        ctx', cs, None
    | S_expr e ->
        let et, cs = expression ctx e in
        ctx, cs, Some et in
  fold_left ~init:(ctx, [], None) ~f:statement typed

let rec substitute s t x =
  match s with
  | T_var v ->
    begin match x with
    | T_var v' when v = v' -> t
    | T_tuple (a, b) -> T_tuple (substitute s t a, substitute s t b)
    | T_fun (a, b) -> T_fun (substitute s t a, substitute s t b)
    | _ -> x
    end
  | _ -> x

let rec occurs s = function
  | t when s = t -> true
  | T_tuple (a, b) -> occurs s a || occurs s b
  | T_fun (a, b) -> occurs s a || occurs s b
  | _ -> false

let rec unify = function
  | [] -> ident
  | (s, t) :: cs when s = t -> unify cs
  | (T_var _ as s, t) :: cs when not (occurs s t) ->
      let cs' = List.map ~f:(fun (a, b) -> (substitute s t a, substitute s t b)) cs in
      Fn.compose (unify cs') (substitute s t)
  | (s, (T_var _ as t)) :: cs when not (occurs t s) ->
      let cs' = List.map ~f:(fun (a, b) -> (substitute t s a, substitute t s b)) cs in
      Fn.compose (unify cs') (substitute t s)
  | (T_fun (s1, s2), T_fun (t1, t2)) :: cs ->
      unify @@ (s1, t1) :: (s2, t2) :: cs
  | (T_tuple (s1, s2), T_tuple (t1, t2)) :: cs ->
      unify @@ (s1, t1) :: (s2, t2) :: cs
  | _ -> raise @@ Typed_exception Unification_failed

let unify_and_substitute ?(ctx=Ctx.empty) typed =
  let open List in
  let open Ctx in
  let ctx, cs, ot = derive_constraints ~ctx typed in
  let sub = unify cs in
  let rec pattern = function
    | P_int _ | P_unit as p -> p
    | P_wildcard t -> P_wildcard (sub t)
    | P_ident (id, t) -> P_ident (id, sub t)
    | P_tuple (a, b) -> P_tuple (pattern a, pattern b)
    | P_constr (c, op) -> P_constr (c, Option.map ~f:pattern op) in
  let rec expression = function
    | E_int _ | E_unit | E_ident _ as e -> e
    | E_apply (f, args, t) -> E_apply (expression f, args >>| expression, sub t)
    | E_tuple (a, b) -> E_tuple (expression a, expression b)
    | E_let (p, e, body) -> E_let (pattern p, expression e, expression body)
    | E_fun (p, body) -> E_fun (pattern p, expression body)
    | E_constr (c, oe) -> E_constr (c, Option.(oe >>| expression))
    | E_match (e, cases, t) ->
        let cases' = cases >>| fun (p, body) -> pattern p, expression body in
        E_match (expression e, cases', sub t) in
  let statement = function
    | S_let (p, e) -> S_let (pattern p, expression e)
    | S_type_decl _ as x -> x
    | S_expr e -> S_expr (expression e) in
  let ctx' = { ctx with bindings = ctx.bindings >>| fun (id, t) -> id, sub t } in
  typed >>| statement, ctx', Option.map ~f:sub ot
