silly-ml
========
[![Build Status](https://travis-ci.org/rootmos/silly-ml.svg?branch=master)](https://travis-ci.org/rootmos/silly-ml)

`silly-ml` (aka [vacation-ml](https://en.wikipedia.org/wiki/Summer)) is a:
* small ML-like, type-checked language,
* both interpreted and compiled to x86-64 assembly,
* created because [it's weekend](https://www.isittheweekendyet.com/) and
* written in [OCaml](https://ocaml.org/) and inspired by
  [OCaml](https://github.com/ocaml/ocaml).

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
>
```
