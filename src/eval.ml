open Core_kernel.Std
open Printf

module Ctx = struct
  type t = {
    typed_ctx: Typed.Ctx.t;
    lambda_ctx: Lambda.Ctx.t;
    interpret_ctx: Interpret.Ctx.t;
  }

  let empty = {
    typed_ctx = Typed.Ctx.empty;
    lambda_ctx = Lambda.Ctx.empty;
    interpret_ctx = Interpret.Ctx.empty;
  }
end

let step ctx s =
  let open Ctx in
  let parsed = Parsed_helpers.parse s in
  let typed = Typed.introduce_types parsed in
  let typed', typed_ctx, ot = Typed.unify_and_substitute ~ctx:ctx.typed_ctx typed in
  let lambda, lambda_ctx = Lambda.transform_to_lambda ~ctx:ctx.lambda_ctx typed' in
  let v, interpret_ctx = Interpret.interpret ~ctx:ctx.interpret_ctx lambda in
  let ctx' = { typed_ctx; lambda_ctx; interpret_ctx } in
  v, ctx', ot

let eval s = step Ctx.empty s |> fun (x, _, _) -> x

let rec repl ?(ctx=Ctx.empty) () =
  let open Ctx in

  let rec pretty ?(wrap=false) v t =
    match v, t with
    | Interpret.I.V_int i, Typed.T_ident id ->
        let td = Typed.Ctx.lookup_type ctx.typed_ctx id in
        Lambda.Ctx.reconstruct_constructor td i
    | Interpret.I.V_tag (i, v'), Typed.T_ident id ->
        let td = Typed.Ctx.lookup_type ctx.typed_ctx id in
        let c = Lambda.Ctx.reconstruct_constructor td i in
        begin match List.Assoc.find ~equal:(=) (List.(td >>| fun (Typed.V_constr x) -> x)) c with
        | Some (Some t) when wrap -> sprintf "(%s %s)" c (pretty ~wrap:true v' t)
        | Some (Some t)           -> sprintf "%s %s" c (pretty ~wrap:true v' t)
        | _ -> failwith "eval whoopsie"
        end
    | v, _ -> Interpret.I.to_string v in

  print_string "> ";
  try
    let s = Pervasives.read_line () in
    let v, ctx', ot = step ctx s in
    begin match (v, ot) with
    | _, None -> ()
    | v, Some t -> printf "%s: %s\n" (pretty v t) (Typed.format_typ t)
    end;
    repl ~ctx:ctx' ()
  with
  | Parsed_helpers.Parser_helpers_exception Parsed_helpers.Parsing ->
      printf "parsing error\n";
      repl ~ctx ()
  | Parsed_helpers.Parser_helpers_exception (Parsed_helpers.Lexing msg) ->
      printf "lexing error: %s\n" msg;
      repl ~ctx ()
  | Interpret.Interpret_exception error ->
      printf "interpreter error: %s\n" (Interpret.format_error error);
      repl ~ctx ()
  | Lambda.Lambda_exception error ->
      printf "lambda error: %s\n" (Lambda.format_error error);
      repl ~ctx ()
  | Typed.Typed_exception error ->
      printf "typed error: %s\n" (Typed.format_error error);
      repl ~ctx ()
  | Sys.Break ->
      Pervasives.print_newline (); repl ~ctx ()
  | End_of_file -> ()
