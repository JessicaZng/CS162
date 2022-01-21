(** Removes all consecutive duplicate elements from a list. *)
let rec compress (xs: 'a list) : 'a list =
  match xs with
  [] -> []
  | (h::t) -> (
    match t with
    [] -> [h]
    | (x::y) -> if h = x then compress t else h::(compress t)    
  )