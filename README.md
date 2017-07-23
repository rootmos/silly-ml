silly-ml
========
[![Build Status](https://travis-ci.org/rootmos/silly-ml.svg?branch=master)](https://travis-ci.org/rootmos/silly-ml)

`silly-ml` (aka [vacation-ml](https://en.wikipedia.org/wiki/Summer)) is a:
* small ML-like, type-checked language,
* interpreted in a REPL or compiled to x86-64 assembly (with garbage getting both marked and sweeped),
* created because [it's weekend](https://www.isittheweekendyet.com/) and because I haven't written one before,
* coded in [OCaml](https://ocaml.org/) and inspired by [OCaml](https://github.com/ocaml/ocaml).

_Disclaimer:_ the silly prefix indicates that this is a hobby project with no other
purpose than to learn and explore and it should definently be interpreted as an
homage to its big brothers.

Usage
-----
Simplest way to try it out is by using Docker:
```
docker run -it rootmos/silly-ml
```

Examples
--------
Here's an [example session](repl.expect) from the REPL:
```
> 7;;
7: int
> type foo = A | B of int;;
> A;;
A: foo
> B 7;;
B 7: foo
> let f x = match x with A -> 0 | B i -> i;;
> f;;
<fun>: foo -> int
> f A;;
0: int
> f (B 3);;
3: int
> type bar = C of foo;;
> C A;;
C A: bar
> f (C A);;
typed error: unification failed
> ((), 7);;
((), 7): (unit, int)
> 3 * (7 - (1 + 2));;
12: int
> let x = 7;;
> print_int x;; print_newline ();;
7
(): unit
> let f x y = x + y;;
> f;;
<fun>: int -> int -> int
> let g = f 1;;
> g;;
<fun>: int -> int
> let h = f 2;;
> h;;
<fun>: int -> int
> g 2;;
3: int
> h 2;;
4: int
> let sum n = let rec go acc i = match i with 0 -> acc | _ -> go (acc + i) (i - 1) in go 0 n;;
> sum;;
<fun>: int -> int
> sum 6;;
21: int
> let rec fib n = match n with 0 -> 0 | 1 -> 1 | n -> (fib (n - 1)) + (fib (n - 2));;
> fib 5;;
5: int
> fib 6;;
8: int
>
```
