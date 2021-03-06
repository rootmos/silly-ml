open Interpret.I
open Eval
open Core_kernel.Std

let%test_unit "eval empty string" =
  [%test_result: value]
  (eval "")
  ~expect:V_unit

let%test_unit "eval 11;;" =
  [%test_result: value]
  (eval "11;;")
  ~expect:(V_int 11)

let%test_unit "eval let x = 7 in x;;" =
  [%test_result: value]
  (eval "let x = 7 in x;;")
  ~expect:(V_int 7)

let%test_unit "eval let y = 7 in y;;" =
  [%test_result: value]
  (eval "let y = 7 in y;;")
  ~expect:(V_int 7)

let%test_unit "eval let (x, y) = (1, ());; (x, y);;" =
  [%test_result: value]
  (eval "let (x, y) = (1, ());; (x, y);;")
  ~expect:(V_tuple (V_int 1, V_unit))

let%test_unit "eval let x = 7;; let (7, y) = (x, ());; let z = y;; z;;" =
  [%test_result: value]
  (eval "let x = 7;; let (7, y) = (x, ());; let z = y;; z;;")
  ~expect:(V_unit)

let%test_unit "eval let f a = a;; f ();;" =
  [%test_result: value]
  (eval "let f a = a;; f ();;")
  ~expect:(V_unit)

let%test_unit "eval let f a = a;; f 7;;" =
  [%test_result: value]
  (eval "let f a = a;; f 7;;")
  ~expect:(V_int 7)

let%test_unit "eval let kite a b = a;; kite () 7;;" =
  [%test_result: value]
  (eval "let kite a b = a;; kite () 7;;")
  ~expect:(V_unit)

let%test_unit "eval let kestrel a b = a;; kestrel () 7;;" =
  [%test_result: value]
  (eval "let kestrel a b = b;; kestrel () 7;;")
  ~expect:(V_int 7)

let%test_unit "eval type foo = A;; let x = A;; A;;" =
  [%test_result: value]
  (eval "type foo = A;; let x = A;; A;;")
  ~expect:(V_int 0)

let%test_unit "eval type foo = A | B;; type bar = A;; let x = B;; let y = A;;" =
  [%test_result: value]
  (eval "type foo = A | B;; type bar = A;; let x = B;; x;;")
  ~expect:(V_int 1)

let%test_unit "eval type foo = A of int;; let x = A 7;; x;;" =
  [%test_result: value]
  (eval "type foo = A of int;; let x = A 7;; x;;")
  ~expect:(V_tag (0, V_int 7))

let%test_unit "eval type foo = A of int * unit;; A (7, ());;" =
  [%test_result: value]
  (eval "type foo = A of int * unit;; A (7, ());;")
  ~expect:(V_tag (0, V_tuple (V_int 7, V_unit)))

let%test_unit "eval let _ = 7;;" =
  [%test_result: value]
  (eval "let _ = 7;;")
  ~expect:(V_unit)

let%test_unit "eval type foo = A;; let A = A;;" =
  [%test_result: value]
  (eval "type foo = A;; let A = A;;")
  ~expect:(V_unit)

let%test_unit "eval type foo = A of int;; let (A i) = A 7;; i;;" =
  [%test_result: value]
  (eval "type foo = A of int;; let (A i) = A 7;; i;;")
  ~expect:(V_int 7)

let%test_unit "eval type foo = A of int | B of int * int;; B (3,4);;" =
  [%test_result: value]
  (eval "type foo = A of int | B of int * int;; B (3,4);;")
  ~expect:(V_tag (1, V_tuple (V_int 3, V_int 4)))

let%test_unit "eval match 7 with 0 -> 1 | _ -> 2;;" =
  [%test_result: value]
  (eval "match 7 with 0 -> 1 | _ -> 2;;")
  ~expect:(V_int 2)

let%test_unit "eval match ((1, 2), 3) with ((1, a), b) -> (a, b) | _ -> (1, 2);;" =
  [%test_result: value]
  (eval "match ((1, 2), 3) with ((1, a), b) -> (a, b) | _ -> (1, 2);;")
  ~expect:(V_tuple (V_int 2, V_int 3))

let%test_unit "eval let x = 7;; and then x;;" =
  [%test_result: value]
  (let (_, ctx, _) = step Ctx.empty "let x = 7;;" in
  step ctx "x;;" |> fun (x, _, _) -> x)
  ~expect:(V_int 7)

let%test_unit "eval 'type foo = A of int | B;; let f x = match x with A i -> i | B -> 0;;' and then 'f ()' should fail" =
  [%test_result: unit]
  (let (_, ctx, _) = step Ctx.empty "type foo = A of int | B;; let f x = match x with A i -> i | B -> 0;;" in
  try let _ = step ctx "f ();;" in failwith "failed!"
  with
  | Typed.Typed_exception Typed.Unification_failed -> ())
  ~expect:()

let%test_unit "eval 'let z = (1, 2);; print_int z;;' should fail" =
  [%test_result: unit]
  (try (let _ = eval "let z = (1, 2);; print_int z;;" in failwith "failed!") with
  | Typed.Typed_exception Typed.Unification_failed -> ())
  ~expect:()

let%test_unit "eval let f x = let y = () in (x + 1);; (f 2) + 3;;" =
  [%test_result: value]
  (eval "let f x = let y = () in (x + 1);; (f 2) + 3;;")
  ~expect:(V_int 6)

let%test_unit "eval 7 + 4;;" =
  [%test_result: value]
  (eval "7 + 4;;")
  ~expect:(V_int 11)

let%test_unit "eval 7 - 4;;" =
  [%test_result: value]
  (eval "7 - 4;;")
  ~expect:(V_int 3)

let%test_unit "eval 3 * 4;;" =
  [%test_result: value]
  (eval "3 * 4;;")
  ~expect:(V_int 12)

let%test_unit "eval let f x y = x + y;; let g = f 1;; let h = f 2;; g 2;;" =
  [%test_result: value]
  (eval "let f x y = x + y;; let g = f 1;; let h = f 2;; g 2;;")
  ~expect:(V_int 3)

let%test_unit "eval type foo = A | B of int;; let f x = match x with A -> 0 | B i -> i;; f (B 3);;" =
  [%test_result: value]
  (eval "type foo = A | B of int;; let f x = match x with A -> 0 | B i -> i;; f (B 3);;")
  ~expect:(V_int 3)

let%test_unit "eval let a = 7;; let f x = a + x;; let 8 = f 1;; let a = 9;; f 1;;" =
  [%test_result: value]
  (eval "let a = 7;; let f x = a + x;; let 8 = f 1;; let a = 9;; f 1;;")
  ~expect:(V_int 8)

let%test_unit "eval let rec go i = match i with 0 -> 10 | j -> go (j - 1) in go 5;;" =
  [%test_result: value]
  (eval "let rec go i = match i with 0 -> 10 | j -> go (j - 1) in go 5;;")
  ~expect:(V_int 10)

let%test_unit "eval let sum n = let rec go acc i = match i with 0 -> acc | _ -> go (acc + i) (i - 1) in go 0 n;; sum 6;;" =
  [%test_result: value]
  (eval "let sum n = let rec go acc i = match i with 0 -> acc | _ -> go (acc + i) (i - 1) in go 0 n;; sum 6;;")
  ~expect:(V_int 21)

let%test_unit "eval let rec fib n = match n with 0 -> 1 | 1 -> 1 | m -> (fib (m - 1)) + (fib (m - 2));; fib 6;;" =
  [%test_result: value]
  (eval "let rec fib n = match n with 0 -> 0 | 1 -> 1 | m -> (fib (m - 1)) + (fib (m - 2));; fib 6;;")
  ~expect:(V_int 8)

let%test_unit "eval let rec fib n = match n with 0 -> 1 | 1 -> 1 | _ -> (fib (n - 1)) + (fib (n - 2));; fib 7;;" =
  [%test_result: value]
  (eval "let rec fib n = match n with 0 -> 0 | 1 -> 1 | _ -> (fib (n - 1)) + (fib (n - 2));; fib 7;;")
  ~expect:(V_int 13)

let () = Ppx_inline_test_lib.Runtime.exit ()
