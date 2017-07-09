silly-ml
========

[![Build Status](https://travis-ci.org/rootmos/silly-ml.svg?branch=master)](https://travis-ci.org/rootmos/silly-ml)

Example session from the REPL:
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
