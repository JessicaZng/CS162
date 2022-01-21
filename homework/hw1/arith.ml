(** Arithmetic expressions *)
type expr = 
    Nat of int (** A natural number constant *)
  | Var of string (** A variable of unknown value *)
  | Add of expr * expr (** Addition *)
  | Mul of expr * expr (** Multiplication *)

(** Example: 5 + (1 * 2) *)
let example_expr = Add(Nat(5), Mul(Nat(1), Nat(2)))
(* (Add (Var "x", Add (Var "x", Mul(Nat 1, Add(Nat 0, Var "y"))))) *)

(* Operations on constants should be simplified, 
  e.g. 1 + (1 * 3) is simplified to 4.
Addition and multiplication identities should be simplified, 
  e.g. 1 * (x + 0 + (5 * 0)) is simplified to x. 
Specifically, you need to handle addition by 0, multiplication by 0, and multiplication by 1.
All other combinations of addition and multiplication should be left as-is.
  For example, you do not need to distribute multiplication (e.g. you should leave 2 * (x + 1) as-is). 
  You should leave expressions such as x + (2 * x) as-is. *)

(** Simplify an arithmetic expression *)
let rec simplify (e : expr) : expr =

  match e with
  (* All additions *)
  Add (x,y) -> (
    let xe = simplify x in 
    let ye = simplify y in 
    match (xe,ye) with 
    | (Nat 0, b) -> b
    | (a, Nat 0) -> a
    | (Nat a, Nat b) -> Nat (a+b)
    | (a,b) -> Add (a,b)
  )
    
  (* All multiplications *)
  | Mul (x,y) -> (
    let xe = simplify x in 
    let ye = simplify y in 
    match (xe,ye) with 
    ((Nat 0),b) -> (Nat 0)
    | (a, (Nat 0)) -> (Nat 0)
    | (Nat 1, b) -> simplify b
    | (a, Nat 1) -> simplify a
    | (Nat a, Nat b) -> Nat (a*b)
    | (a,b) -> Mul (a,b)
  )
  | ee -> ee


  (* oepration on constants *)
  (* addition by 0 *)
  (* multiplication by 0, 1 *)


(*simplify (Add (Mul (Nat 5, Nat 3), Mul (Nat 6, Nat 2)));;*)
(* #use "arith.ml";; *)

(* let rec longest_path (t: 'a tree) : 'a list =
  match t with
  | Leaf -> []
  | Node (l, r, x) ->
    let pl = longest_path l in
    let pr = longest_path r in
    let longer = 
      if List.length pl > List.length pr
      then pl
      else pr in
    x :: longer *)