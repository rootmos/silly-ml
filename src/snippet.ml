open Core_kernel.Std

let run s =
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
  (*Interpret.format_value x |> print_endline;*)

  print_newline ()

let () = Config.set_verbose true

let () = run "let f a = a;; f ();;"
(*let () = run "type foo = A | B of int;; let f x = match x with A -> 0 | B i -> i;; f (B 3);;"*)
let () = run "let f x y = x + y;; let g = f 1;; let h = f 2;; g 2;;"
