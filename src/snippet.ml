open Core_kernel.Std

let go s =
  print_endline s;

  let parsed = Parsed_helpers.parse s in
  Parsed.sexp_of_t parsed |> Sexp.to_string_hum |> print_endline;

  let typed = Typed.introduce_types parsed in
  Typed.sexp_of_t typed |> Sexp.to_string_hum |> print_endline;

  let _, cs, _ = Typed.derive_constraints typed in
  Typed.sexp_of_constrs cs |> Sexp.to_string_hum |> print_endline;

  let typed', _, _ = Typed.unify_and_substitute typed in
  Typed.sexp_of_t typed' |> Sexp.to_string_hum |> print_endline;

  let lambda, _ = Lambda.transform_to_lambda typed' in
  Lambda.sexp_of_t lambda |> Sexp.to_string_hum |> print_endline;

  (*let x, _ = Interpret.interpret lambda in*)
  (*Interpret.I.to_string x |> print_endline;*)

  let anf = Anf.transform_to_anf lambda in
  Anf.sexp_of_t anf |> Sexp.to_string_hum |> print_endline;

  (*let y, ctx = Backend.go anf in*)
  (*Backend.sexp_of_listing y |> Sexp.to_string_hum |> print_endline;*)
  (*Backend.Ctx.sexp_of_t ctx |> Sexp.to_string_hum |> print_endline;*)

  (*Backend.anf_to_asm anf |> print_string;*)

  Pervasives.print_newline ()

let run s = Errors.run_with_pretty_errors (fun () -> go s)

(*let () = Config.set_verbose true*)

let () = run "let rec go i = match i with 0 -> 10 | j -> go (j - 1) in go 5;;"
(*let () = run "let sum n = let rec go acc i = match i with 0 -> acc | _ -> go (acc + i) (i - 1) in go 0 n;;"*)
(*let () = run "let rec fib n = match n with 0 -> 1 | 1 -> 1 | _ -> (fib (n - 1)) + (fib (n - 2));; fib 7;;"*)
