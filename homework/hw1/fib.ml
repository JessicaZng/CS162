(** Computes the n-th Fibonacci number. *)
let rec fib (n: int) : int =
  match n with
    0 -> 0
    | 1 -> 1
    | _ -> fib (n-1) + fib (n-2)

  (* another valid method:   
    if n = 0 then 0
    else if n = 1 then 1
    else fib(n-1) + fib(n-2)
  *)