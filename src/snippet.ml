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

  let lambda = Lambda.transform_to_lambda typed' in
  Lambda.sexp_of_t lambda |> Sexp.to_string |> print_endline;

  let x = Interpret.interpret lambda in
  Interpret.sexp_of_value x |> Sexp.to_string |> print_endline;

  print_newline ();;


run "let x = 7 in x;;";

run "7;;";
run "let y = 7 in y;;";
run "let (x, y) = (1, ());; (x, y);;";

run "let x = 7;; let (1, y) = (x, ());; let z = y;;";
run "let f a = a;; let () = f ();;";
run "let kite a b = a;; let () = kite () 7;;";

run "type foo = A;; let x = A;;";
run "type foo = A | B;; type bar = A;; let x = B;; let y = A;;";

run "type foo = A of int;; let x = A 7;;";
run "type foo = A of int * unit;; let x = A (7, ());;";

run "let _ = 7;;";

run "type foo = A;; let A = A;;";
run "type foo = A of int;; let A i = A 7;;";

run "let () = match 7 with 0 -> () | _ -> ();;";

run "let _ = match ((1, 2), 3) with ((1, a), b) -> (a, b) | _ -> (1, 2);;";

(*run "let f = 7;; let () = f () 2";*)
(*run "let kite a b = a;; let () = kite () 7;; let 5 = kite () 7";*)
