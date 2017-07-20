open Core_kernel.Std
open Printf

let run_with_pretty_errors ?(err=raise) f =
  try f () with 
  | Parsed_helpers.Parser_helpers_exception Parsed_helpers.Parsing as e ->
      eprintf "parsing error\n";
      err e
  | Parsed_helpers.Parser_helpers_exception (Parsed_helpers.Lexing msg) as e ->
      eprintf "lexing error: %s\n" msg;
      err e
  | Interpret.Interpret_exception error as e ->
      eprintf "interpreter error: %s\n" (Interpret.format_error error);
      err e
  | Lambda.Lambda_exception error as e ->
      eprintf "lambda error: %s\n" (Lambda.format_error error);
      err e
  | Typed.Typed_exception error as e ->
      eprintf "typed error: %s\n" (Typed.format_error error);
      err e
  | Anf.Anf_exception error as e ->
      eprintf "anf error: %s\n" (Anf.format_error error);
      err e
  | Backend.Backend_exception error as e ->
      eprintf "backend error: %s\n" (Backend.format_error error);
      err e
