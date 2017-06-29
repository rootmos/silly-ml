open Core_kernel.Std

type identifier = Identifier of string
[@@deriving sexp]

type expression = Int of int
[@@deriving sexp]

type typ = Type of identifier | Tuple of typ * typ
[@@deriving sexp]

type variant = Variant of string | VariantOf of (string * typ)
[@@deriving sexp]

type type_decl = variant list
[@@deriving sexp]

type statement =
    Let of (identifier * expression)
  | TypeDecl of (identifier * type_decl)
[@@deriving sexp]

type t = statement list
[@@deriving sexp]

let compare_sexpr = Pervasives.compare
