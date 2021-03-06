open Core_kernel.Std
open Printf

let silly_to_asm input output =
  let source = In_channel.input_all input in
  let parsed = Parsed_helpers.parse source in

  let typed = Typed.introduce_types parsed in
  if Config.verbose () then begin
    Typed.sexp_of_t typed
      |> Sexp.to_string_hum
      |> printf "typed (not unified):\n%s\n";

    let _, cs, _ = Typed.derive_constraints typed in
    Typed.sexp_of_constrs cs
      |> Sexp.to_string_hum
      |> printf "constraints:\n%s\n";
  end;

  let typed', _, _ = Typed.unify_and_substitute typed in
  if Config.verbose () then
    Typed.sexp_of_t typed'
      |> Sexp.to_string_hum
      |> printf "typed (unified):\n%s\n";
  let lambda, _ = Lambda.transform_to_lambda typed' in
  let anf = Anf.transform_to_anf lambda in
  let asm = Backend.anf_to_asm anf in
  Out_channel.output_string output asm

let exit_with_msg msg =
  Pervasives.prerr_endline msg;
  exit 1

let asm_to_binary ld_search_path asm_fname bin_fname =
  let obj_fname = Filename.chop_extension asm_fname ^ ".o" in

  let as_cmd = sprintf "as -g -o %s %s" obj_fname asm_fname in
  Sys.command as_cmd |> fun exit_code ->
    if exit_code = 0 then ()
    else exit_with_msg (sprintf "as failed when executing: %s" as_cmd);

  let ld_cmd = sprintf "ld -static -L%s -o %s %s -lruntime"
    ld_search_path bin_fname obj_fname in
  Sys.command ld_cmd |> fun exit_code ->
    if exit_code = 0 then ()
    else exit_with_msg (sprintf "ld failed when executing: %s" ld_cmd)

let () =
  let open Arg in
  let output = ref "a.out" in
  let input = ref None in
  let only_compile = ref false in
  let ld_search_path = ref "." in
  let spec = [
    ("-o", Set_string output, "output");
    ("-c", Set only_compile, "only compile, don't assemble and link");
    ("-L", Set_string ld_search_path, "library search path");
    ("-v", Unit (fun _ -> Config.set_verbose true), "verbose");
  ] in
  parse spec (fun i -> input := Some i) "silly-ml to x64 compiler";

  match !input with
  | None -> failwith "no input file"
  | Some source ->
      let asm_fname = Filename.chop_extension source ^ ".asm" in
      In_channel.with_file source ~f:(fun i ->
        Out_channel.with_file asm_fname ~f:(fun o ->
          Errors.run_with_pretty_errors ~err:(fun _ -> exit 1) (fun () ->
            silly_to_asm i o)));

      if not !only_compile then
        asm_to_binary !ld_search_path asm_fname !output;
