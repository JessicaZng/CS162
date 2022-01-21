(** Returns the maximum of an integer list if the list is non-empty,
  * or None otherwise.
  *)
let rec max (ns: int list) : int option =
  match ns with
  [] -> None
  | (h::t) -> (
    match t with
    [] -> Some h
    | (hd::tl) -> if (h > hd) then (max (h::tl)) else (max (hd::tl))
  )
      
    (* let rec max_helper h t = 
    match t with
    [] -> Some h
    | (hd::tl) -> if (h > hd) then (max_helper h tl) else (max_helper hd tl);; *)

    (* For this problem, i referenced this website for the idea: 
https://stackoverflow.com/questions/58157626/finding-max-min-number-in-list-in-ocaml *)