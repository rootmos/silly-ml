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
| Lea of label * operand
| Add of operand * operand
| Sub of operand * operand
| Mul of operand * operand
| Cmp of operand * operand
| Shr of int * operand
| Shl of int * operand
| Call of label
| Push of operand
| Pop of register
| Jmp of operand
| Je of operand
| Jne of operand
| Set_label of label
| Comment of string
| Ret
[@@deriving sexp]

type listing = op list
[@@deriving sexp]

type labelled_listing = {
  global: bool;
  label: label;
  code: listing
} [@@deriving sexp]

type error =
  Register_error of register
| Unsupported_primitive_function of string
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
  let lea l o = L.tell [ Lea (l, o) ]
  let add o1 o2 = L.tell [ Add (o1, o2) ]
  let sub o1 o2 = L.tell [ Sub (o1, o2) ]
  let mul o1 o2 = L.tell [ Mul (o1, o2) ]
  let cmp o1 o2 = L.tell [ Cmp (o1, o2) ]
  let shr n o = L.tell [ Shr (n ,o) ]
  let shl n o = L.tell [ Shl (n ,o) ]
  let set_label l = L.tell [ Set_label l ]
  let jne o = L.tell [ Jne o ]
  let je o = L.tell [ Je o ]
  let jmp o = L.tell [ Jmp o ]
  let ret = L.tell [ Ret ]
  let comment s = L.tell [ Comment s ]

  let const i = Constant i
  let reg r = Register r
  let deref b r = Dereference (b, r)
  let derefw w r = deref (words w) r
  let label l = Label l

  let call_registers = [RDI; RSI; RDX; RCX; R8; R9]

  let call l args =
    let open List in
    let rs = take call_registers (length args) in
    let args = zip_exn args rs >>| fun (o, r) ->
      if o = Register r then [] else [ Mov (o, Register r) ] in
    L.tell @@ concat args @ [Call l]

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
  let run (a, ls) = a, ls
  let run_ t = run t |> snd
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

  let run ?(init=empty) t =
    let a, s = t init in
    let ls =
      if s.size > 0
      then Listing.(
        sub (const s.size) (reg rsp) >>
        insert s.ls >>
        add (const s.size) (reg rsp) |> run_)
      else Listing.(insert s.ls |> run_ ) in
    a, ls

  let run_ ?(init=empty) t =
    run ~init t |> snd

  let ret t = run_ t @ [ Ret ]
end

type closure = {
  capture: labelled_listing;
  eval: labelled_listing
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

let mk_int i = (i lsl 1) lor 0b1

let rec go_value ~current_closure ?(target=Register RAX) v =
  let open Local in
  match v with
  | A.V_int i ->
      mov (const (mk_int i)) target
  | A.V_tuple (a, b) ->
      declare >>= fun m ->
      mallocw 3 >>
      mov (reg rax) m >>

      declare >>= fun ma ->
      go_value ~current_closure ~target:ma a >>

      declare >>= fun mb ->
      go_value ~current_closure ~target:mb b >>

      mov m (reg rax) >>
      mov (const 0) (derefw 0 rax) >>
      mov ma (derefw 1 rax) >>
      mov mb (derefw 2 rax) >>

      mov (reg rax) target
  | A.V_unit -> go_value ~current_closure ~target @@ A.V_int 0
  | A.V_ident id ->
      call "lookup_identifier" [current_closure; const id] >>
      mov (reg rax) target
  | _ -> failwith "not implemented: go_value"

module Closure_struct = struct
  type t = {
    pattern_captures: int list;
    offsets: (int * int) list;
  }

  let mk { A.uc_p; A.uc_free } =
    let pattern_captures = A.pattern_captures uc_p in
    let offsets = uc_free @ pattern_captures |> List.dedup
      |> List.mapi ~f:(fun o id -> (id, o)) in
    { pattern_captures; offsets }

  let key_offset o = words ((2*o) + 2)
  let value_offset o = words ((2*o) + 3)

  let key o r = Local.(deref (key_offset o) r)
  let value o r = Local.(deref (value_offset o) r)

  let code_addr r = Local.(derefw 0 r)
  let continuation r = Local.(derefw 1 r)

  let size t = words ((2 * List.length t.offsets) + 2)

  let captured_by_pattern t id = List.mem t.pattern_captures id ~equal:(=)

  let find t id = List.Assoc.find_exn t.offsets id ~equal:(=)

  let map t ~f = List.map t.offsets ~f
end

module Abort = struct
  let messages = [
    "__match_error_msg", "match error\\n";
  ]

  let abort = {
    global = false; label = "__abort";
    code = Local.(
      define (reg rdi) >>= fun code ->
      define (reg rsi) >>= fun msg ->
      call "strlen" [msg] >>
      call "write" [const 2; msg; reg rax] >>
      call "exit" [code]
    ) |> Local.run_
  }

  let match_error = {
    global = false; label = "__match_error";
    code = Listing.(
      lea "__match_error_msg" (reg rsi) >>
      call abort.label [const 2; reg rsi]
    ) |> Listing.run_
  }
end

let rec mk_pattern_match ~str ~abort
  ?(value=Register RDI) ?(closure=RSI) p =
  let open Listing in
  match p with
  | A.P_ident id ->
      let o = Closure_struct.find str id in
      mov value (Closure_struct.value o closure) |> run_
  | A.P_wildcard -> []
  | A.P_int i ->
      cmp (const (mk_int i)) value >>
      jne abort |> run_
  | _ -> failwith "pattern not implemented"

let rec mk_closure ~current_closure ~ctx uc =
  let str = Closure_struct.mk uc in
  let l = sprintf "__f_%d" (fresh_identifier ()) in
  let body, ctx = go
    ~current_closure:(Register RSI)
    ~current_continuation:(Register RDX) ~ctx uc.A.uc_body in
  let c = {
    capture = { global = false; label = l ^ "_capture";
      code = Local.(
        comment (sprintf "Capturing:\n%s"
          (Anf.sexp_of_uncaptured_closure uc |> Sexp.to_string_hum)) >>
        declare >>= fun new_capture ->
        define (reg rdi) >>= fun parent_offsets ->
        malloc (Closure_struct.size str) >>
        mov (reg rax) new_capture >>
        comment "store address of generated evaluation code" >>
        lea l (reg rcx) >>
        mov (reg rcx) (Closure_struct.code_addr rax) >>
        mov (reg rdx) (Closure_struct.continuation rax) >>
        let os = Closure_struct.map str ~f:(fun (id, o) ->
          mov new_capture (reg rbx) >>
          mov (const id) (Closure_struct.key o rbx) >>
          if Closure_struct.captured_by_pattern str id
          then mov (const 0) (Closure_struct.value o rbx)
          else
            call "lookup_identifier" [parent_offsets; const id] >>
            mov new_capture (reg rbx) >>
            mov (reg rax) (Closure_struct.value o rbx)) in
        all_ignore os >> mov new_capture (reg rax)) |> Local.ret
    };
    eval = {
      global = false; label = l; code =
        let comment = [
          Comment (sprintf "Evaluating:\n%s\n\n"
            (Anf.sexp_of_expression uc.A.uc_body |> Sexp.to_string_hum))
        ] in
        let pattern = mk_pattern_match ~str
          ~abort:(Label Abort.match_error.label) uc.A.uc_p in
        List.concat [comment; pattern; body]
    }
  } in
  c, Ctx.add ctx c
and go ~current_closure ~current_continuation ?(ctx=Ctx.empty) l =
  let call_continuation k arg =
    Listing.(
      comment "calling continuation" >>
      mov arg (reg rdi) >>
      mov k (reg rsi) >>
      mov (derefw 1 rsi) (reg rdx) >>
      jmp (derefw 0 rsi) |> run_) in
  match l with
  | A.E_value v ->
      let (arg, cc), l = Local.(
        define (current_continuation) >>= fun cc ->
        go_value ~current_closure v >>
        mov cc (reg rdx) >> return ((reg rax), (reg rdx))
        ) |> Local.run in
      (l @ call_continuation cc arg), ctx
  | A.E_this_and_then { A.this; A.and_then } ->
      let c, ctx = mk_closure ~current_closure ~ctx and_then in
      let (current_closure, next_continuation), ls = Local.(
        define current_closure >>= fun current_closure ->
        comment "capturing \"then\", ie next continuation" >>
        call c.capture.label [current_closure; current_continuation] >>
        mov (reg rax) (reg rdx) >>
        mov current_closure (reg rsi) >>
        return (reg rsi, reg rdx)) |> Local.run in
      let ctx, ls' = Listing.(
        comment "evaluate \"this\"" >>
        let l, ctx = go ~current_closure
          ~current_continuation:next_continuation ~ctx this in
        insert l >> return ctx |> run) in
      ls @ ls', ctx
  | A.E_uncaptured_closure uc ->
      let c, ctx = mk_closure ~current_closure ~ctx uc in
      let l = Listing.(
        call c.capture.label [current_closure; Constant 0] |> run_) in
      let l' = call_continuation current_continuation (Register RAX) in
      l @ l', ctx
  | A.E_apply (a, b) ->
      let jo, ls = Local.(
        define (reg rsi) >>= fun current_closure ->
        comment "remember current continuation" >>
        define current_continuation >>= fun cc ->
        declare >>= fun ma ->
        comment "evaluate function" >>
        go_value ~current_closure ~target:ma a >>
        comment "evaluate argument" >>
        go_value ~current_closure ~target:(reg rdi) b >>
        comment "call function using current continuation" >>
        mov ma (reg rsi) >>
        mov cc (reg rdx) >>
        return @@ deref 0 rsi
      ) |> Local.run in
      ls @ [Jmp jo], ctx
  | A.E_primitive ("%plus%", [a; b]) ->
      let (arg, cc), l = Local.(
        define (reg rsi) >>= fun current_closure ->
        define current_continuation >>= fun cc ->
        declare >>= fun ma ->
        comment "fetch first operand" >>
        go_value ~current_closure ~target:ma a >>
        shr 1 ma >>
        comment "fetch second operand" >>
        go_value ~current_closure ~target:(reg rdi) b >>
        shr 1 (reg rdi) >>
        comment "I think therefore I sum..." >>
        add ma (reg rdi) >>
        shl 1 (reg rdi) >>
        mov cc (reg rdx) >>
        return (reg rdi, reg rdx)) |> Local.run in
      (l @ call_continuation cc arg), ctx
  | A.E_primitive ("%minus%", [a; b]) ->
      let (arg, cc), l = Local.(
        define (reg rsi) >>= fun current_closure ->
        define current_continuation >>= fun cc ->
        declare >>= fun ma ->
        comment "fetch first operand" >>
        go_value ~current_closure ~target:ma a >>
        shr 1 ma >>
        comment "fetch second operand" >>
        go_value ~current_closure ~target:(reg rdi) b >>
        shr 1 (reg rdi) >>
        sub (reg rdi) ma >>
        mov ma (reg rdi) >>
        shl 1 (reg rdi) >>
        mov cc (reg rdx) >>
        return (reg rdi, reg rdx)) |> Local.run in
      (l @ call_continuation cc arg), ctx
  | A.E_primitive ("%times%", [a; b]) ->
      let (arg, cc), l = Local.(
        define (reg rsi) >>= fun current_closure ->
        define current_continuation >>= fun cc ->
        declare >>= fun ma ->
        comment "fetch first operand" >>
        go_value ~current_closure ~target:ma a >>
        shr 1 ma >>
        comment "fetch second operand" >>
        go_value ~current_closure ~target:(reg rdi) b >>
        shr 1 (reg rdi) >>
        mul ma (reg rdi) >>
        shl 1 (reg rdi) >>
        mov cc (reg rdx) >>
        return (reg rdi, reg rdx)) |> Local.run in
      (l @ call_continuation cc arg), ctx
  | A.E_primitive ("%exit%", [a]) ->
      Local.(
        go_value ~current_closure ~target:(reg rdi) a >>
        shr 1 (reg rdi) >>
        call "exit" [reg rdi]) |> Local.run_, ctx
  | A.E_primitive ("%print_int%", [a]) ->
      let (arg, cc), l = Local.(
        define (reg rsi) >>= fun current_closure ->
        define current_continuation >>= fun cc ->
        comment "fetch operand" >>
        go_value ~current_closure ~target:(reg rdi) a >>
        shr 1 (reg rdi) >>
        comment "convert to string" >>
        call "itos" [reg rdi] >>
        comment "write to stdout" >>
        call "write" [const 1; reg rax; reg rbx] >>
        (* TODO: add asserts *)
        mov cc (reg rdx) >>
        return (const 0, reg rdx)) |> Local.run in
      (l @ call_continuation cc arg), ctx
  | A.E_primitive ("%print_newline%", [_]) ->
      let (arg, cc), l = Local.(
        define (reg rsi) >>= fun current_closure ->
        define current_continuation >>= fun cc ->
        call "write_newline" [const 1] >>
        (* TODO: add asserts *)
        mov cc (reg rdx) >>
        return (const 0, reg rdx)) |> Local.run in
      (l @ call_continuation cc arg), ctx
  | A.E_primitive (pf, _) ->
      raise @@ Backend_exception (Unsupported_primitive_function pf)
  | _ -> failwith "not implemented: go"


let lookup_identifier = {
  global = false;
  label = "lookup_identifier";
  code = Listing.(
    let id = reg rsi in
    mov (reg rdi) (reg rbx) >>
    add (const (words 2)) (reg rbx) >>
    set_label "lookup_identifier_loop" >>
    cmp (deref 0 rbx) id >>
    je (label "lookup_identifier_done") >>
    add (const (words 2)) (reg rbx) >>
    jmp (label "lookup_identifier_loop") >>
    set_label "lookup_identifier_done" >>
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
    | Dereference (0, r) -> sprintf "(%s)" (register r)
    | Dereference (i, r) -> sprintf "%d(%s)" i (register r)

  let op = function
    | Mov (o1, o2) -> sprintf "movq %s, %s" (operand o1) (operand o2)
    | Lea (l, o)   -> sprintf "leaq %s, %s" l (operand o)
    | Add (o1, o2) -> sprintf "addq %s, %s" (operand o1) (operand o2)
    | Sub (o1, o2) -> sprintf "subq %s, %s" (operand o1) (operand o2)
    | Mul (o1, o2) -> sprintf "imulq %s, %s" (operand o1) (operand o2)
    | Cmp (o1, o2) -> sprintf "cmpq %s, %s" (operand o1) (operand o2)
    | Shr (n, o) -> sprintf "shrq $%d, %s" n (operand o)
    | Shl (n, o) -> sprintf "shlq $%d, %s" n (operand o)
    | Call s -> sprintf "call %s" s
    | Push o -> sprintf "pushq %s" (operand o)
    | Pop r -> sprintf "popq %s" (register r)
    | Jmp ((Dereference _) as o)  -> sprintf "jmp *%s" (operand o)
    | Jmp o -> sprintf "jmp %s" (operand o)
    | Jne ((Dereference _) as o)  -> sprintf "jne *%s" (operand o)
    | Jne l -> sprintf "jne %s" (operand l)
    | Je ((Dereference _) as o)  -> sprintf "je *%s" (operand o)
    | Je l -> sprintf "je %s" (operand l)
    | Ret -> sprintf "ret"
    | Set_label l -> sprintf "%s:" l
    | Comment s -> String.split_lines s
        |> List.map ~f:(fun s -> "# " ^ s) |> String.concat ~sep:"\n"

  let indentation = "  "

  let indent = function
    | Set_label _ -> ""
    | Comment _ -> ""
    | _ -> indentation

  let listing = List.map ~f:(fun o -> indent o ^ op o)

  let labelled_listing { global; label; code } =
    List.concat [
      [ "" ];
      if global then [".global " ^ label] else [];
      [label ^ ":"];
      listing code
    ]

  let closure { capture; eval } =
    labelled_listing capture @ labelled_listing eval

  let of_ctx ctx =
    List.map ~f:closure ctx.Ctx.closures
      |> List.concat
      |> String.concat ~sep:"\n"

  let of_string (l, s) = [
    sprintf "%s:" l;
    sprintf "%s.asciz \"%s\"" indentation s;
  ]

  let of_strings ss = List.map ~f:of_string ss |> List.concat
end

let anf_to_asm l =
  let code, ctx = go
    ~current_closure:(Constant 0)
    ~current_continuation:(Register RDX) l in
  let exit_closure = {
    global = false; label = "__exit";
    code = Local.(call "exit" [const 0]) |> Local.run_
  } in
  let exit_capture = {
    global = false; label = exit_closure.label ^ "_capture";
    code = Local.(
      mallocw 2 >>
      lea exit_closure.label (reg rcx) >>
      mov (reg rcx) (derefw 0 rax) >>
      mov (const 0) (derefw 1 rax)
    ) |> Local.ret
  } in
  let main = {
    global = true; label = "main";
    code = Local.(
      call exit_capture.label [] >> mov (reg rax) (reg rdx) >>
      insert code |> run_)
  } in
  let d = Output.of_strings Abort.messages
  and l = Output.of_ctx ctx
  and l' = List.map ~f:(fun ll ->
    Output.labelled_listing ll |> String.concat ~sep:"\n")
  [
    Abort.match_error;
    Abort.abort;
    lookup_identifier;
    exit_closure;
    exit_capture;
    main;
  ] in
  String.concat ~sep:"\n" @@ List.concat [
    [ ".data" ];
    d;
    [ ".text" ];
    [ l ];
    l';
    [ "" ]
  ]

let format_error = function
| Register_error r -> sprintf "register error %s" (Output.register r)
| Unsupported_primitive_function pf ->
    sprintf "unsupported primitive function %s" pf
