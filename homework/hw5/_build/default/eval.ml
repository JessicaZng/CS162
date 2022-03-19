open Ast

(** Helper function for parsing an expression. Useful for testing. *)
let parse (s: string) : Ast.expr =
  Parser.main Scanner.token (Lexing.from_string s)

(*******************************************************************
 **********************   Interpreter   ****************************
 *******************************************************************
 *******************************************************************)

(** Exception indicating that evaluation is stuck *)
exception Stuck of string

(* Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(* let rec eval e : expr = e *)

(** Variable set. Based on OCaml standard library Set. *)
module VarSet = Set.Make (String)

(** Computes the set of free variables in the given expression *)
let rec free_vars (e : expr) : VarSet.t =
  match e with
  Var s -> VarSet.singleton s
  | Lambda (s, t, e1) -> VarSet.diff (free_vars e1) (free_vars (Var s))
  | App (e1, e2) -> VarSet.union (free_vars e1) (free_vars e2)
  | LetBind (s, t, e1, e2) -> VarSet.union (free_vars e1) (VarSet.diff (free_vars e2) (free_vars (Var s)))
  | Fix e1 -> free_vars e1

  | NumLit n -> VarSet.empty
  | Binop (e1, op, e2) -> VarSet.union (free_vars e1) (free_vars e2)
  | IfThenElse (e1, e2, e3) -> VarSet.union (VarSet.union (free_vars e1) (free_vars e2)) (free_vars e3)
  | ListNil t -> VarSet.empty
  | ListCons (e1, e2) -> VarSet.union (free_vars e1) (free_vars e2)
  | ListHead e1 -> free_vars e1
  | ListTail e1 -> free_vars e1
  | ListIsNil e1 -> free_vars e1

(* a helper function for finding the next available name *)
let rec alpha_renaming (x : string) (n : int) (v : VarSet.t) : string =
  if (VarSet.mem (x ^ (string_of_int n)) v)
    then (alpha_renaming x (n+1) v)
  else (x ^ (string_of_int n))

(** Performs the substitution [x -> e1]e2 *)
let rec subst (x : string) (e1 : expr) (e2 : expr) : expr =
match e2 with
Var s -> if s = x then e1 else e2
(* [x->e1]/s. e21
if x is bounded by s then do nothing
else if s is in FV e1 then [x->e1]/s0. e21 of s0
else subst x e1 e21
*)
| Lambda (s, t, e21) -> (
  if s = x then Lambda (s, t, e21)  
  else if (VarSet.mem s (free_vars e1)) 
    then let s' = (alpha_renaming s 0 (VarSet.union (free_vars e1) (free_vars e21))) in 
    (subst x e1 (Lambda (s', t, (subst s (Var s') e21))))
  else Lambda (s, t, (subst x e1 e21))
)
  
| App (e21, e22) -> App ((subst x e1 e21), (subst x e1 e22))
(* [x->e1]let s = e21 in e22 => [x->e1]/(s.e22 e21) => 
 *)
| LetBind (s, t, e21, e22) -> (
  let e' = subst x e1 (App (Lambda (s, t, e22), e21)) in
  match e' with
  App (Lambda (s', t', e22'), e21') -> LetBind (s', t', e21', e22')
  | _ -> im_stuck "x"
  )
| Fix e -> Fix (subst x e1 e)
| NumLit n -> NumLit n
| Binop (e21, op, e22) -> Binop ((subst x e1 e21), op, (subst x e1 e22))
| IfThenElse (e21, e22, e23) -> IfThenElse ((subst x e1 e21), (subst x e1 e22), (subst x e1 e23))
| ListNil t -> ListNil t
| ListCons (e21, e22) -> ListCons ((subst x e1 e21), (subst x e1 e22))
| ListHead e -> ListHead (subst x e1 e)
| ListTail e -> ListTail (subst x e1 e)
| ListIsNil e -> ListIsNil (subst x e1 e)


(** Evaluates e. You need to copy over your
   implementation of homework 3. *)
let rec eval (e : expr) : expr =
  try
    match e with
    (* Things you need to implement in hw4 *)
    Var s -> im_stuck "variable"
    
    | Lambda (s, t, e1) -> Lambda (s, t, e1)
    
    | App (e1, e2) -> (
      match eval e1 with 
        Lambda (s, t, e) -> eval (subst s (eval e2) e) 
        | _ -> im_stuck "e1 is not in lambda x.e format"
      )

    | LetBind (s, t, e1, e2) -> eval (App (Lambda (s, t, e2), e1))
    
    | Fix e1 -> (
      match eval e1 with 
      Lambda (f, t, e1') -> eval (subst f (Fix (Lambda (f, t, e1'))) e1')
      | _ -> im_stuck "not in lambda x.e format"
    )


    (* --- Start of hw3 --- *)
    | NumLit n -> NumLit n
    
    | Binop (e1, op, e2) -> (
      let e1' = eval e1 in let e2' = eval e2 in 
      match (e1', e2') with 
      (* after evaluates, e1 and e2 become NumLit type *)
      (NumLit n1, NumLit n2) -> (
        if op = Add then NumLit (n1 + n2) 
        else if op = Sub then NumLit (n1 - n2) 
        else if op = Mul then NumLit (n1 * n2)  
        else if op = Gt then (if (n1 > n2) then NumLit 1 else NumLit 0)
        else if op = Lt then (if (n1 < n2) then NumLit 1 else NumLit 0)
        else if op = And then (if (n1 <> 0 && n2 <> 0) then NumLit 1 else NumLit 0)
        else if op = Or then (if (n1 <> 0 || n2 <> 0) then NumLit 1 else NumLit 0)
        else if op = Eq then (if (n1 = n2) then NumLit 1 else NumLit 0)
        else im_stuck "op not recognized"
      )
      (* otherwiese exception *)
      | _ -> im_stuck "e1 or e2 are not NumLit Type"
    )

    | IfThenElse (e1, e2, e3) -> (
      let e1' = eval e1 in 
      match e1' with 
      NumLit n1 -> (if (n1 <> 0) then (eval e2) else (eval e3))
      | _ -> im_stuck "e1 is not NumLit Type"
    )
    (* How to deal with ListNil t *)
    | ListNil t -> ListNil t
    
    | ListCons (e1, e2) -> (
      let e1' = eval e1 in let e2' = eval e2 in ListCons (e1', e2')
    )

    | ListHead e1 ->  (
      match eval e1 with
      ListCons (e1', _) -> e1'
      | _ -> im_stuck "?"
    )
    
    | ListTail e1 ->  (
      match eval e1 with
      ListCons (_, e1') -> e1'
      | _ -> im_stuck "???"
    )
    
    | ListIsNil e1 -> (
      match eval e1 with 
      ListNil t -> NumLit 1
      | ListCons (_, _) -> NumLit 0
      | _ -> im_stuck "???"
    )
    (* --- End of hw3 --- *)

    | _ -> im_stuck "end"
  with
  | Stuck msg -> im_stuck (msg ^ "\nin expression " ^ string_of_expr e)
  ;;
