open Core_kernel.Std
module A = Anf

type register = RAX | RBX | RCX | RDX | R8 | R9 | R10 | RDI | RSI
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
| Jmp of label
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

let words n = 8 * n

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

let mk_closure { A.uc_p; A.uc_free; A.uc_body } =
  let pattern_captures = A.pattern_captures uc_p in
  let offsets = uc_free @ pattern_captures |> List.dedup
    |> List.mapi ~f:(fun o id -> (id, o)) in
  let i = fresh_identifier () in
  let label = sprintf "__f_%d" i in
  {
    capture = {
      label = sprintf "__f_%d_capture" i;
      code =
        let parent_offsets = RDX in
        let capture_reg = RBX in
        let dereference_key o =
          Dereference (words ((2*o) + 1), capture_reg) in
        let dereference_value o =
          Dereference (words ((2*o) + 2), capture_reg) in
        let size = words @@ (2 * List.length offsets) + 1 in
        let ls = malloc
          ~precious:[parent_offsets]
          ~target:(Register capture_reg) size in
        let ls = ls @ [ Mov (Label label, Dereference (0, capture_reg)) ] in
        let os = List.map offsets ~f:(fun (id, o) ->
          Mov (Constant id, dereference_key o) ::
            if List.mem pattern_captures id ~equal:(=)
            then [ Mov (Constant 0, dereference_value o) ]
            else call
              ~precious:[capture_reg; parent_offsets]
              ~target:(dereference_value o)
              "lookup_identifier" [Register parent_offsets; Constant id]) in
        let ls = ls @ List.concat os in
        ls @ [ Mov (Register capture_reg, Register RAX); Ret ]
    };
    eval = {
      label;
      code = []
    };
    offsets
  }

let rec go_value ?(target=RAX) = function
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
  | A.V_ident _ -> []
  | _ -> failwith "not implemented: go_value"

let rec go ?(ctx=Ctx.empty) ?(k="__done") l =
  match l with
  | A.E_value v -> go_value v, ctx
  | A.E_this_and_then { A.this; A.and_then } ->
      let _, ctx = go ~ctx and_then.uc_body in
      let c = mk_closure and_then in
      let ctx = Ctx.add ctx c in
      let l, ctx = go ~ctx this in
      l @ [Jmp c.eval.label], ctx
  | A.E_uncaptured_closure uc ->
      let c = mk_closure uc in
      let ctx = Ctx.add ctx c in
      [Call c.capture.label], ctx
  | A.E_apply (a, b) ->
      [], ctx
  | _ -> failwith "not implemented: go"

(*let output ctx =*)
