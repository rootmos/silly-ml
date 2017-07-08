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
  let (typed', typed_ctx') = Typed.unify_and_substitute ~ctx:ctx.typed_ctx typed in
  let (lambda, lambda_ctx') = Lambda.transform_to_lambda ~ctx:ctx.lambda_ctx typed' in
  let (v, interpret_ctx') = Interpret.interpret ~ctx:ctx.interpret_ctx lambda in
  (v, { typed_ctx = typed_ctx'; lambda_ctx = lambda_ctx'; interpret_ctx = interpret_ctx' })

let eval s = step Ctx.empty s |> fst
