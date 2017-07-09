silly-ml
========
[![Build Status](https://travis-ci.org/rootmos/silly-ml.svg?branch=master)](https://travis-ci.org/rootmos/silly-ml)

`silly-ml` is a small ML-like, type-checked, interpreted language created because [it's weekend](https://www.isittheweekendyet.com/),
written in [OCaml](https://ocaml.org/) and is inspired by [OCaml](https://github.com/ocaml/ocaml).

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
> f (A);;
0: int
> f (B 3);;
3: int
> type bar = C of foo;;
> C A;;
C A: bar
> f (C A);;
typed error: unification failed
>
```
