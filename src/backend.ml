open Core_kernel.Std
module A = Anf

type register = RAX | RBX | RCX | RDX | R8 | R9 | R10 | RDI | RSI
[@@deriving sexp]

type operand =
  Register of register
| Constant of int
| Dereference of int * register
[@@deriving sexp]

type op =
  Mov of operand * operand
| Call of string
| Push of operand
| Pop of register
[@@deriving sexp]

type listing = op list
[@@deriving sexp]

let malloc size =
  [ Mov (Constant size, Register RDI); Call "malloc" ]

let words n = 8 * n

module Ctx = struct
  type t = {
    closures: int list;
    applys: int list;
  }

  let empty = { closures = []; applys = [] }
end

let rec go l =
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
    | _ -> failwith "not implemented: go_value" in
  match l with
  | A.E_value v -> go_value v
  | _ -> failwith "not implemented: go"
