open Core_kernel.Std

let run s =
  print_endline s;

  let parsed = Parsed_helpers.parse s in
  Parsed.sexp_of_t parsed |> Sexp.to_string |> print_endline;

  let typed = Typed.introduce_types parsed in
  Typed.sexp_of_t typed |> Sexp.to_string |> print_endline;

  let cs = Typed.derive_constraints typed in
  Typed.sexp_of_constrs cs |> Sexp.to_string |> print_endline;

  let typed' = Typed.unify_and_substitute typed in
  Typed.sexp_of_t typed' |> Sexp.to_string |> print_endline;

  print_newline ();;

run "let x = 7";
run "let x = let y = 7 in y";
run "let (x, y) = (1, ())";
run "let f a = a;; let () = f ()";
run "let kite a b = a;; let () = kite () 7";
run "let kite a b = a;; let () = kite () 7;; let 5 = kite () 7";
run "let f = 7;; let () = f () 2";
