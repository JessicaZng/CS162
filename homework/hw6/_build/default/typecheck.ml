open Ast

module Env = Map.Make (String) type env = typ Env.t

exception Type_error of string 
let ty_err msg = raise (Type_error msg)

let todo () = failwith "TODO"


let rec typecheck (env : env) (e : expr) : typ =
  try
  match e with
  Var s -> (
    match (Env.find_opt s env) with
    Some t -> t
    | None -> ty_err "not a type"
  ) (* ok *)
  
  | Lambda (s, topt, e1) -> ( 
    match topt with
    Some t1 -> TFun (t1, (typecheck (Env.add s t1 env) e1))
    | _ -> ty_err "not a type"
  ) (* ok *)
    
  | App (e1, e2) -> (
    match (typecheck env e1) with 
    TFun (t1, t2) -> (if (typecheck env e2) = t1 then t2 
      else ty_err "not substutable")
    | _ -> ty_err "e1 is not a fun"
  )
  | LetBind (s, topt, e1, e2) -> typecheck env (App (Lambda (s, topt, e2), e1))
  
  | Fix e1 -> (
    match (typecheck env e1) with 
    TFun(t1, t2) when t1 = t2 -> t1 
    | _ -> ty_err "dif ele type"
  )

  | NumLit n -> TInt 
    (* ok *)
  
  | Binop (e1, op, e2) -> (
    let t1 = (typecheck env e1) in 
    if (typecheck env e2) = t1 then (
      if t1 = TInt then TInt 
      else if op = Eq then TInt 
      else ty_err "not int"
      ) 
    else ty_err "type dif"
  ) (* ok *)

  | IfThenElse (e1, e2, e3) -> (
    if (typecheck env e1) = TInt && (typecheck env e2) = (typecheck env e3) 
    then (typecheck env e2) else ty_err "invalid"
  ) (* ok *)

  | ListNil topt -> (
    match topt with
    Some t -> TList t
    | None -> ty_err "not a type"
  ) (* ok *)

  | ListCons (e1, e2) -> (
    let t1 = (typecheck env e1) in
    if (typecheck env e2) = (TList t1) then (TList t1) else ty_err "dif ele type in 1 list"
  ) (* ok *)

  | ListHead e1 -> (
    match (typecheck env e1) with 
    (TList t) -> t 
    | _ -> ty_err "not in list of T format"
  ) (* ok *)

  | ListTail e1 -> (
    match (typecheck env e1) with 
    (TList t) -> (TList t)
    | _ -> ty_err "not in list of T format"
  ) (* ok *)

  | ListIsNil e1 -> (
    match (typecheck env e1) with 
    (TList t) -> TInt 
    | _ -> ty_err "not in list of T format"
  ) (* ok *)

  with
  | Type_error msg -> ty_err (msg ^ "\nin expression " ^ string_of_expr e)


