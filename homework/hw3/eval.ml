open Ast

(* Helper function for parsing an expression. Useful for testing. *)
let parse (s: string) : Ast.expr =
  Parser.main Scanner.token (Lexing.from_string s)
(*******************************************************************|
|**********************   Interpreter   ****************************|
|*******************************************************************|
|*******************************************************************)

(* Exception indicating that evaluation is stuck *)
exception Stuck of string

(* Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(* Raises an exception for things that need to be implemented
 * in this assignment *)
let todo () = failwith "TODO"

(* Raises an exception for things to be in the next assignment *)
let hw4 () = failwith "Homework 4"

(* Helper function to check that an expression is a value, otherwise raises a
   Stuck exception. *)
let assert_value e =
  if is_value e then () else im_stuck (string_of_expr e ^ " is not a value")

(* Evaluates expression e *)
let rec eval (e : expr) : expr =
  try
    match e with
    (* Things you need to implement *)
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
      | (Var _, _) -> Binop (e1', op, e2')
      | (_, Var _) -> Binop (e1', op, e2')
      (* otherwiese exception *)
      | _ -> im_stuck "e1 or e2 are not NumLit Type"
    )

    | IfThenElse (e1, e2, e3) -> (
      let e1' = eval e1 in 
      match e1' with 
      NumLit n1 -> (if (n1 <> 0) then (eval e2) else (eval e3))
      | _ -> im_stuck "e1 is not NumLit Type"
    )
    (* How to deal with ListNil *)
    | ListNil -> ListNil
    
    | ListCons (e1, e2) -> (
      let e1' = eval e1 in let e2' = eval e2 in ListCons (e1', e2')
    )

    | ListHead e ->  (
      let e' = eval e in
      match e' with
      ListCons (e1', _) -> e1'
      | _ -> im_stuck "?"
    )
    
    | ListTail e ->  (
      let e' = eval e in
      match e' with
      ListCons (_, e2') -> e2'
      | _ -> im_stuck "???"
    )
    
    | ListIsNil e -> (
      let e' = eval e in
      match e' with 
      ListNil -> NumLit 1
      | ListCons (_, _) -> NumLit 0
      | _ -> im_stuck "???"
    )
    (* Things you don't need to implement in this assignment *)
    | _ -> hw4 ()
  with
  | Stuck msg -> im_stuck (msg ^ "\nin expression " ^ string_of_expr e)
