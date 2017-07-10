open Core_kernel.Std

let run s =
  print_endline s;

  let parsed = Parsed_helpers.parse s in
  Parsed.sexp_of_t parsed |> Sexp.to_string |> print_endline;

  let typed = Typed.introduce_types parsed in
  Typed.sexp_of_t typed |> Sexp.to_string |> print_endline;

  let _, cs, _ = Typed.derive_constraints typed in
  Typed.sexp_of_constrs cs |> Sexp.to_string |> print_endline;

  let typed', _, _ = Typed.unify_and_substitute typed in
  Typed.sexp_of_t typed' |> Sexp.to_string |> print_endline;

  let lambda, _ = Lambda.transform_to_lambda typed' in
  Lambda.sexp_of_t lambda |> Sexp.to_string |> print_endline;

  let x, _ = Interpret.interpret lambda in
  Interpret.format_value x |> print_endline;

  print_newline ();;


run "1 + 2;;"
