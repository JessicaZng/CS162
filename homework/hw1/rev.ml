(** Concatenates two lists. *)
let rec cat (xs: 'a list) (ys: 'a list) : 'a list =
  match xs with
  [] -> ys
  | (h::t) -> h::(cat t ys)

(** Reverses a list. *)
let rec rev (xs: 'a list) : 'a list =
  match xs with
  [] -> []
  | (h::t) -> cat (rev t) [h]

  (* For this problem, i referenced this website: 
  https://www.cs.umd.edu/class/fall2015/cmsc330/lectures/ocaml-basics.pdf *)

