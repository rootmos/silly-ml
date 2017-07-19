open Core_kernel.Std
open Printf
module A = Anf

type register = RAX | RBX | RCX | RDX | R8 | R9 | R10 | RDI | RSI | RSP
[@@deriving sexp]

let call_registers = [RDI; RSI; RDX; RCX; R8; R9]

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
| Add of operand * operand
| Sub of operand * operand
| Cmp of operand * operand
| Call of string
| Push of operand
| Pop of register
| Jmp of operand
| Jne of operand
| Set_label of label
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

module type Listing_intf = sig
  type 'a t
  val tell : op list -> unit t
end

module Asm_syntax(L: Listing_intf) = struct
  let rax = RAX
  let rbx = RBX
  let rcx = RCX
  let rdx = RDX
  let r8  = R8
  let r9  = R9
  let r10 = R10
  let rdi = RDI
  let rsi = RSI
  let rsp = RSP

  let mov o1 o2 = L.tell @@ if o1 = o2 then [] else [ Mov (o1, o2) ]
  let sub o1 o2 = L.tell [ Sub (o1, o2) ]
  let add o1 o2 = L.tell [ Add (o1, o2) ]
  let cmp o1 o2 = L.tell [ Cmp (o1, o2) ]
  let set_label l = L.tell [ Set_label l ]
  let jne o = L.tell [ Jne o ]
  let jmp o = L.tell [ Jmp o ]
  let ret = L.tell [ Ret ]

  let const i = Constant i
  let reg r = Register r
  let deref b r = Dereference (b, r)
  let derefw w r = deref (words w) r
  let label l = Label l

  let call l args =
    let open List in
    let rs = take call_registers (length args) in
    let args = zip_exn args rs  >>| fun (o, r) -> Mov (o, Register r) in
    L.tell @@ args @ [Call l]

  let malloc size = call "malloc" [Constant size]
  let mallocw w = malloc (words w)
end

module Listing = struct
  type 'a t = 'a * op list

  include Monad.Make(struct
    type 'a t = 'a * op list
    let return x = x, []
    let map (x, ls) ~f = f x, ls
    let map = `Custom map
    let bind (x, ls) ~f = let (y, ls') = f x in y, ls @ ls'
  end)

  let insert ops = (), ops

  include Asm_syntax(struct
    type 'a t = 'a * op list
    let tell = insert
  end)

  let (>>) x y = x >>= fun () -> y
  let run_ (_, ls) = ls
end

module Local = struct
  type scope = {
    ls: op list;
    size: int;
  }

  let empty = { ls = []; size = 0 }

  include Monad.Make(struct
    type 'a t = scope -> 'a * scope
    let return x = fun s -> (x, s)
    let map = `Define_using_bind
    let bind (ma: 'a t) ~(f: 'a -> 'b t) =
      fun s -> let a, s' = ma s in (f a) s'
  end)

  let get: scope -> scope * scope = fun s -> s, s
  let put s = fun _ -> (), s
  let modify f = fun s -> (), f s

  let insert ops = modify @@ fun s -> { s with ls = s.ls @ ops }

  include Asm_syntax(struct
    type 'a t = scope -> 'a * scope
    let tell = insert
  end)

  let (>>) x y = x >>= fun () -> y

  let declare =
    get >>= fun s ->
    put {s with size = s.size + words 1 } >>
    return @@ deref s.size rsp

  let define v =
    declare >>= fun o ->
    mov v o >>
    return o

  let run_ ?(init=empty) t =
    let _, s = t init in
    Listing.(
      sub (const s.size) (reg rsp) >>
      insert s.ls >>
      add (const s.size) (reg rsp) |> run_)

  let ret t = run_ t @ [ Ret ]
end

type closure = {
  capture: labelled_listing;
  eval: labelled_listing;
  offsets: (int * int) list
} [@@deriving sexp]

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

let rec go_value ?(current_closure=Register RBX) ?(target=Register RAX) v =
  let open Local in
  match v with
  | A.V_int i ->
      mov (const ((i lsl 1) lor 0b1)) target
  | A.V_tuple (a, b) ->
      declare >>= fun m ->
      mallocw 3 >>
      mov (reg rax) m >>

      declare >>= fun ma ->
      go_value ~target:ma a >>

      declare >>= fun mb ->
      go_value ~target:mb b >>

      mov m (reg rax) >>
      mov (const 0) (derefw 0 rax) >>
      mov ma (derefw 1 rax) >>
      mov mb (derefw 2 rax) >>

      mov (reg rax) target
  | A.V_unit -> go_value ~target @@ A.V_int 0
  | A.V_ident id ->
      call "lookup_identifier" [current_closure; const id] >>
      mov (reg rax) target
  | _ -> failwith "not implemented: go_value"

let rec mk_closure ~ctx { A.uc_p; A.uc_free; A.uc_body } =
  let pattern_captures = A.pattern_captures uc_p in
  let offsets = uc_free @ pattern_captures |> List.dedup
    |> List.mapi ~f:(fun o id -> (id, o)) in
  let i = fresh_identifier () in
  let l = sprintf "__f_%d" i in
  let key_offset o = words ((2*o) + 1) in
  let value_offset o = words ((2*o) + 2) in
  let body, ctx = go ~ctx uc_body in
  {
    capture = {
      label = sprintf "__f_%d_capture" i;
      code =
        Local.(
          declare >>= fun new_capture ->
          define (Register RDI) >>= fun parent_offsets ->
          mallocw ((2 * List.length offsets) + 1) >>
          mov (reg rax) new_capture >>
          mov (label l) (derefw 0 rax) >>
          let os = List.map offsets ~f:(fun (id, o) ->
            mov new_capture (reg rbx) >>
            mov (const id) (deref (key_offset o) rbx) >>
            if List.mem pattern_captures id ~equal:(=)
            then mov (const 0) (deref (value_offset o) rbx)
            else
              call "lookup_identifier" [parent_offsets; const id] >>
              mov new_capture (reg rbx) >>
              mov (reg rax) (deref (value_offset o) rbx)) in
          all_ignore os >> mov new_capture (reg rax)) |> Local.ret
    };
    eval = {
      label = l;
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
  | A.E_value v -> Local.run_ (go_value v), ctx
  | A.E_this_and_then { A.this; A.and_then } ->
      let _, ctx = go ~ctx and_then.A.uc_body in
      let c, ctx = mk_closure ctx and_then in
      let ctx = Ctx.add ctx c in
      let l, ctx = go ~ctx this in
      let ls = Local.(
        define (reg rdi) >>= fun current_closure ->
        call c.capture.label [reg rdi] >>
        insert l >>
        mov current_closure (reg rdi)
      ) |> Local.run_ in
      ls @ [Jmp (Label c.eval.label)], ctx
  | A.E_uncaptured_closure uc ->
      let c, ctx = mk_closure ctx uc in
      let ctx = Ctx.add ctx c in
      Listing.(call c.capture.label [reg rdi] |> run_), ctx
  | A.E_apply (a, b) ->
      let ls = Local.(
        declare >>= fun ma ->
        go_value ~target:ma a >>
        go_value ~target:(reg rax) b
      ) |> Local.run_ in
      ls @ [Jmp (Dereference (0, RBX))], ctx
  | _ -> failwith "not implemented: go"


let lookup_identifier = {
  label = "lookup_identifier";
  code = Listing.(
    let id = reg rdi in
    mov (reg rdi) (reg rbx) >>
    add (const (words 1)) (reg rbx) >>
    set_label "lookup_identifier_loop" >>
    cmp (deref 0 rbx) id >>
    jne (label "lookup_identifier_loop") >>
    mov (derefw 1 rbx) (reg rax) >>
    ret |> run_)
}

module Output = struct
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
    | Cmp (o1, o2) -> sprintf "cmpq %s %s" (operand o1) (operand o2)
    | Call s -> sprintf "call %s" s
    | Push o -> sprintf "pushq %s" (operand o)
    | Pop r -> sprintf "popq %s" (register r)
    | Jmp o -> sprintf "jmp %s" (operand o)
    | Jne l -> sprintf "jne %s" (operand l)
    | Ret -> sprintf "ret"
    | Set_label l -> sprintf "%s:" l

  let indent = function
    | Set_label _ -> ""
    | _ -> "  "

  let listing = List.map ~f:(fun o -> indent o ^ op o)

  let labelled_listing { label; code } =
    [""; label ^ ":"] @ listing code

  let closure { capture; eval } =
    labelled_listing capture @ labelled_listing eval

  let of_ctx ctx =
    List.map ~f:closure ctx.Ctx.closures
      |> List.concat
      |> String.concat ~sep:"\n"
end

let anf_to_asm l =
  let code, ctx = go l in
  String.concat ~sep:"\n" [
    Output.of_ctx ctx;
    Output.labelled_listing lookup_identifier
      |> String.concat ~sep:"\n";
    Output.labelled_listing { label = "main"; code }
      |> String.concat ~sep:"\n"
  ]
