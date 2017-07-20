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

  let anf = Anf.transform_to_anf lambda in
  Anf.sexp_of_t anf |> Sexp.to_string_hum |> print_endline;

  (*let x, _ = Interpret.interpret lambda in*)
  (*Interpret.I.to_string x |> print_endline;*)

  (*let y, ctx = Backend.go anf in*)
  (*Backend.sexp_of_listing y |> Sexp.to_string_hum |> print_endline;*)
  (*Backend.Ctx.sexp_of_t ctx |> Sexp.to_string_hum |> print_endline;*)

  Backend.anf_to_asm anf |> print_string;

  Pervasives.print_newline ()

let run s = Errors.run_with_pretty_errors (fun () -> go s)

let () = Config.set_verbose true

let () = run "let f a = a;; f ();; exit 7;;"
(*let () = run "let f a b = a + b;; f 1 2;;"*)
(*let () = run "print_int 3;;"*)
(*let () = run "(1, 2);;"*)

(*let () = run "type foo = A | B of int;; let f x = match x with A -> 0 | B i -> i;; f (B 3);;"*)
(*let () = run "let f x y = x + y;; let g = f 1;; let h = f 2;; g 2;;"*)

