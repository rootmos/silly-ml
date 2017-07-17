open Core_kernel.Std

let set_minus xs ys =
  List.filter xs ~f:(fun x -> not (List.mem ys x ~equal:(=)))
