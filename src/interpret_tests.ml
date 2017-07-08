open Interpret
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
  (let (_, ctx) = step Ctx.empty "let x = 7;;" in
  step ctx "x;;" |> fst)
  ~expect:(V_int 7)

let%test_unit "eval 'type foo = A of int | B;; let f x = match x with A i -> i | B -> 0;;' and then 'f ()' should fail" =
  [%test_result: unit]
  (let (_, ctx) = step Ctx.empty "type foo = A of int | B;; let f x = match x with A i -> i | B -> 0;;" in
  try let _ = step ctx "f ();;" in failwith "failed!"
  with
  | Typed.Typed_exception Typed.Unification_failed -> ())
  ~expect:()
