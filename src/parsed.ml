open Core_kernel.Std

type identifier = Identifier of string
[@@deriving sexp]

type expression = Int of int
[@@deriving sexp]

type statement = Let of (identifier * expression)
[@@deriving sexp]

type t = statement list
[@@deriving sexp]

let compare_sexpr = Pervasives.compare
