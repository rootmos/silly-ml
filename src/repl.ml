let () =
  Sys.catch_break true;
  Eval.repl ()
