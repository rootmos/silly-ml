open Core_kernel.Std
open Printf
module A = Anf

type register = RAX | RBX | RCX | RDX | R8 | R9 | R10 | RDI | RSI | RSP
[@@deriving sexp]

type label = string
[@@deriving sexp]

type operand =
  Register of register
| Constant of int
| Label of label
| Dereference of int * register
[@@deriving sexp]

type op =
  Mov of operand * operand
| Call of string
| Push of operand
| Pop of register
| Jmp of operand
| Add of operand * operand
| Sub of operand * operand
| Ret
[@@deriving sexp]

type listing = op list
[@@deriving sexp]

type labelled_listing = {
  label: label;
  code: listing
} [@@deriving sexp]

type error = Register_error of register
exception Backend_exception of error


let push_pop ~precious l =
  let open List in
  (precious >>| fun r -> Push (Register r))
    @ l
    @ (rev precious >>| fun r -> Pop r)

let words n = 8 * n

module Local = struct
  type t = int String.Map.t

  let scope_open init =
    let t = List.mapi init ~f:(fun i (n, v) -> (n, words i))
      |> String.Map.of_alist_exn in
    let size = words @@ String.Map.length t in
    let ls = [Sub (Constant size, Register RSP)] in
    let ls = ls @ List.mapi init ~f:(fun i (n, v) ->
      Mov (v, Dereference (words i, RSP))) in
    t, ls

  let scope_close t =
    let size = words @@ String.Map.length t in
    [Add (Constant size, Register RSP)]

  let lookup t n =
    let offset = String.Map.find_exn t n in
    Dereference (offset, RSP)
end

let call_registers = [RDI; RSI; RDX; RCX; R8; R9]

let call ?(target=Register RAX) ?(precious=[]) l args =
  let open List in
  let rs = take call_registers (length args) in
  let args = zip_exn args rs >>| fun (o, r) -> Mov (o, Register r) in
  match target with
  | Dereference (_, RAX) -> raise @@ Backend_exception (Register_error RAX)
  | Register RAX -> push_pop ~precious @@ args @ [Call l]
  | o -> push_pop ~precious @@ args @ [Call l; Mov (Register RAX, target)]

let malloc ?(target=Register RAX) ?(precious=[]) size =
  call ~target ~precious "malloc" [Constant size]


type closure = {
  capture: labelled_listing;
  eval: labelled_listing;
  offsets: (int * int) list
} [@@deriving sexp]

type node = {
  eval: labelled_listing
}

module Ctx = struct
  type t = {
    closures: closure list;
  } [@@deriving sexp]

  let empty = { closures = [] }

  let add { closures } c =
    let closures = c :: closures in { closures }
end

let counter = ref 0
let fresh_identifier () =
  let id = !counter in
  incr counter; id

let fresh_continuation_label =
  sprintf "__k_%d" (fresh_identifier ())

let rec go_value ?(precious=[]) ?(target=RAX) = function
  | A.V_int i ->
      let j = (i lsl 1) lor 0b1 in
      [Mov (Constant j, Register target)]
  | A.V_tuple (a, b) ->
      let m = malloc @@ words 3
      and al = go_value ~target:RBX a
      and bl = go_value ~target:RCX b in
      let l = m @ al @ bl @ [
        Mov (Constant 0, Dereference (0, RAX));
        Mov (Register RBX, Dereference (words 1, RAX));
        Mov (Register RCX, Dereference (words 2, RAX));
      ] in
      begin match target with
      | RAX ->
          (Push (Register RAX)) :: l @ [
            Mov (Register RAX, Register target); Pop RAX]
      | _ -> l
      end
  | A.V_unit -> go_value @@ A.V_int 0
  | A.V_ident id ->
      call ~precious "lookup_identifier" [Register RBX; Constant id]
  | _ -> failwith "not implemented: go_value"

let rec mk_closure ~ctx { A.uc_p; A.uc_free; A.uc_body } =
  let pattern_captures = A.pattern_captures uc_p in
  let offsets = uc_free @ pattern_captures |> List.dedup
    |> List.mapi ~f:(fun o id -> (id, o)) in
  let i = fresh_identifier () in
  let label = sprintf "__f_%d" i in
  let key_offset o = words ((2*o) + 1) in
  let value_offset o = words ((2*o) + 2) in
  let body, ctx = go ~ctx uc_body in
  {
    capture = {
      label = sprintf "__f_%d_capture" i;
      code =
        let local, ls = Local.scope_open [
          ("parent_offset", Register RDI);
          ("new_capture", Constant 0);
        ] in
        let parent_offsets = Local.lookup local "parent_offset" in
        let new_capture = Local.lookup local "new_capture" in
        let size = words @@ (2 * List.length offsets) + 1 in
        let ls = ls @ malloc ~target:new_capture size in
        let ls = ls @ [ Mov (Label label, Dereference (0, RAX)) ] in
        let os = List.map offsets ~f:(fun (id, o) ->
          [ Mov (new_capture, Register RBX);
            Mov (Constant id, Dereference (key_offset o, RBX)) ] @
            if List.mem pattern_captures id ~equal:(=)
            then [ Mov (Constant 0, Dereference (value_offset o, RBX)) ]
            else
              call "lookup_identifier" [parent_offsets; Constant id]
                @ [ Mov (new_capture, Register RBX);
                    Mov (Register RAX, (Dereference (value_offset o, RBX)))]  ) in
        let ls = ls @ List.concat os in
        let ls = ls @ [ Mov (new_capture, Register RAX) ] in
        let ls = ls @ Local.scope_close local in
        ls @ [ Ret ]
    };
    eval = {
      label;
      code =
        [
          match uc_p with
          | A.P_ident id ->
              let o = List.Assoc.find_exn offsets id ~equal:(=) in
              Mov (Register RDI, (Dereference (value_offset o, RSI)))
          | _ -> failwith "pattern not implemented"
        ] @ body
    };
    offsets
  }, ctx
and go ?(ctx=Ctx.empty) ?(precious=[]) ?(k="__done") l =
  match l with
  | A.E_value v -> go_value v, ctx
  | A.E_this_and_then { A.this; A.and_then } ->
      let _, ctx = go ~ctx and_then.uc_body in
      let c, ctx = mk_closure ctx and_then in
      let ctx = Ctx.add ctx c in
      let l, ctx = go ~ctx this in
      call ~precious:[RDI] c.capture.label [Register RDI]
        @ l
        @ [Jmp (Label c.eval.label)], ctx
  | A.E_uncaptured_closure uc ->
      let c, ctx = mk_closure ctx uc in
      let ctx = Ctx.add ctx c in
      call ~precious c.capture.label [Constant 0], ctx
  | A.E_apply (a, b) ->
      go_value ~target:RBX a @ go_value ~precious:[RBX] ~target:RAX b @ [
        Jmp (Dereference (0, RBX))
      ], ctx
  | _ -> failwith "not implemented: go"

module Output = struct
  let indent s = "  " ^ s

  let register = function
    | RAX -> "%rax"
    | RBX -> "%rbx"
    | RCX -> "%rcx"
    | RDX -> "%rdx"
    | RDI -> "%rdi"
    | RSI -> "%rsi"
    | R8  -> "%r8"
    | R9  -> "%r9"
    | R10 -> "%r10"
    | RSP -> "%rsp"

  let operand = function
    | Register r -> register r
    | Constant i -> sprintf "$%d" i
    | Label l -> l
    | Dereference (i, r) -> sprintf "%d(%s)" i (register r)

  let op = function
    | Mov (o1, o2) -> sprintf "movq %s %s" (operand o1) (operand o2)
    | Add (o1, o2) -> sprintf "addq %s %s" (operand o1) (operand o2)
    | Sub (o1, o2) -> sprintf "subq %s %s" (operand o1) (operand o2)
    | Call s -> sprintf "call %s" s
    | Push o -> sprintf "pushq %s" (operand o)
    | Pop r -> sprintf "popq %s" (register r)
    | Jmp o -> sprintf "jmp %s" (operand o)
    | Ret -> sprintf "ret"

  let listing = List.map ~f:(Fn.compose indent op)

  let labelled_listing { label; code } = [""; label ^ ":"] @ listing code

  let closure { capture; eval } =
    labelled_listing capture @ labelled_listing eval

  let of_ctx ctx =
    List.map ~f:closure ctx.Ctx.closures
      |> List.concat
      |> String.concat ~sep:"\n"
end

let anf_to_asm l =
  let code, ctx = go l in
  let main = Output.labelled_listing { label = "main"; code }
    |> String.concat ~sep:"\n" in
  Output.of_ctx ctx ^ "\n" ^ main
