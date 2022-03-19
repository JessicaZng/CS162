open Ast

exception Type_error = Typecheck.Type_error ;;
let ty_err = Typecheck.ty_err ;;

(** Type AST used for type inference *)
type ityp =
  | (* Type variable *)
    IInt
  | (* Function type *)
    IFun of ityp * ityp
  | (* List type *)
    IList of ityp
  | (* Integer type *)
    IVar of int

module Dsl = struct
  let i = IInt
  let (=>) t1 t2 = IFun (t1, t2)
  let l t = IList t
  let v n = IVar n
end

(** Converts a typ into an ityp *)
let rec ityp_of_typ = function
  | TInt -> IInt
  | TFun (t1, t2) -> IFun (ityp_of_typ t1, ityp_of_typ t2)
  | TList t -> IList (ityp_of_typ t)

(** Converts an ityp to a string *)
let rec string_of_ityp : ityp -> string =
  let is_complex = function
    | IFun _ -> true
    | _ -> false
  in
  let pp_nested t =
    let s = string_of_ityp t in if is_complex t then "(" ^ s ^ ")" else s
  in
  function
  | IVar i -> Format.sprintf "X%d" i
  | IInt -> "Int"
  | IFun (t1, t2) -> pp_nested t1 ^ " -> " ^ string_of_ityp t2
  | IList t -> Format.sprintf "List[%s]" (pp_nested t)

(** Typing environment module *)
module Env = Map.Make (String) ;;
type env = ityp Env.t

(** Type inference context. Tracks the typing environment, the generated
   constraints, and a counter for fresh variables.*)
module Context = struct
  type t = Context of env * (ityp * ityp) list * int

  (** The empty type inference context *)
  let empty = Context (Env.empty, [], 0)

  (** Returns the typing environment *)
  let env (Context (e, _, _)) = e

  (** Returns the list of constraints *)
  let cons (Context (_, c, _)) = c

  (** Return the next fresh variable (number) and the updated context *)
  let mk_fresh_var (Context (e, c, i)) = (i, Context (e, c, i + 1))

  (** Modify the environment with the given function, returning the updated context *)
  let modify_env f (Context (e, c, i)) = Context (f e, c, i)

  (** Add the equation t1 = t2 to the context, returning the updated context *)
  let add_eqn t1 t2 (Context (e, c, i)) = Context (e, (t1, t2) :: c, i)
end

(** Generate constraints for type inference. Returns the type (or type variable) *)
let rec gen_cons (ctx : Context.t) (e : expr) : ityp * Context.t =
  try
    match e with
    (* CT-Int *)
    | NumLit _ -> (IInt, ctx)

    (* CT-Var *)
    | Var x -> begin match Env.find_opt x (Context.env ctx) with
        | Some t -> (t, ctx)
        | None -> ty_err ("Unbound variable " ^ x)
      end

    (* CT-Arith, CT-Logic, CT-Ineq *)  
    | Binop (e1, (Add | Sub | Mul | Gt | Lt | And | Or), e2) ->
      let (t1, ctx1) = gen_cons ctx e1 in
      let (t2, ctx2) = gen_cons ctx1 e2 in
      let ctx3 = ctx2 |> Context.add_eqn t1 IInt
                 |> Context.add_eqn t2 IInt
      in
      (IInt, ctx3)

    (* CT-Eq *)
    | Binop (e1, Eq, e2) -> 
      let (t1, ctx1) = gen_cons ctx e1 in
      let (t2, ctx2) = gen_cons ctx1 e2 in
      let ctx3 = ctx2 |> Context.add_eqn t1 t2 in
      (IInt, ctx3)

    (* CT-If *)
    | IfThenElse (e1, e2, e3) -> 
      let (t1, ctx1) = gen_cons ctx e1 in
      let (t2, ctx2) = gen_cons ctx1 e2 in
      let (t3, ctx3) = gen_cons ctx2 e3 in
      let ctx4 = ctx3 |> Context.add_eqn t1 IInt
                 |> Context.add_eqn t2 t3
      in
      (t2, ctx4)
      
    (* CT-Lambda *)
    | Lambda (s, topt, e1) -> begin 
      match topt with 
      | Some t1 -> 
        let x1 = ityp_of_typ t1 in
        let ctx1 = Context.modify_env (Env.add s x1) ctx in
        let (t1, ctx2) = gen_cons ctx1 e1 in
        (IFun (x1, t1), ctx2)

      | None -> 
        let (i1, ctx1) = Context.mk_fresh_var ctx in
        let x1 = Dsl.v i1 in
        let ctx2 = Context.modify_env (Env.add s x1) ctx1 in
        let (t1, ctx3) = gen_cons ctx2 e1 in
        let ctx4 = Context.modify_env (Env.remove s) ctx3 in
        (IFun (x1, t1), ctx4)
      end
    
    (* CT-App *)
    | App (e1, e2) -> 
      let (t1, ctx1) = gen_cons ctx e1 in
      let (t2, ctx2) = gen_cons ctx1 e2 in
      let (i1, ctx3) = Context.mk_fresh_var ctx2 in
      let (i2, ctx4) = Context.mk_fresh_var ctx3 in
      let x1 = Dsl.v i1 in
      let x2 = Dsl.v i2 in
      let ctx5 = ctx4 |> Context.add_eqn t1 (IFun (x1, x2))
                 |> Context.add_eqn t2 x1 
      in
      (x2, ctx5)
    
    (* CT-Fix *)
    | Fix e1 ->
      let (i1, ctx1) = Context.mk_fresh_var ctx in
      let x1 = Dsl.v i1 in
      let (t1, ctx2) = gen_cons ctx1 e1 in
      let ctx3 = ctx2 |> Context.add_eqn t1 (IFun (x1, x1)) in
      (x1, ctx3)

    (* CT-Let *)
    | LetBind (s, topt, e1, e2) -> gen_cons ctx (App (Lambda (s, topt, e2), e1))

    (* CT-Nil *)
    | ListNil topt -> begin match topt with
      Some t1 -> 
        let x1 = ityp_of_typ t1 in
        (IList x1, ctx)
      | None -> 
        let (i1, ctx1) = Context.mk_fresh_var ctx in
        let x1 = Dsl.v i1 in
        (IList x1, ctx1)
    end
    
    (* CT-cons *)
    | ListCons (e1, e2) ->
      let (t1, ctx1) = gen_cons ctx e1 in
      let (t2, ctx2) = gen_cons ctx1 e2 in
      let ctx3 = ctx2 |> Context.add_eqn t2 (IList t1) in
      (IList t1, ctx3)

    (* CT-IsNil *)
    | ListIsNil e1 ->
      let (t1, ctx1) = gen_cons ctx e1 in
      let (i1, ctx2) = Context.mk_fresh_var ctx1 in
      let x1 = Dsl.v i1 in
      let ctx3 = ctx2 |> Context.add_eqn t1 (IList x1) in
      (IInt, ctx3)
  
    (* CT-Head *)
    | ListHead e1 -> 
      let (t1, ctx1) = gen_cons ctx e1 in
      let (i1, ctx2) = Context.mk_fresh_var ctx1 in
      let x1 = Dsl.v i1 in
      let ctx3 = ctx2 |> Context.add_eqn t1 (IList x1) in
      (x1, ctx3)

    (* CT-Tail *)
    | ListTail e1 -> 
      let (t1, ctx1) = gen_cons ctx e1 in
      let (i1, ctx2) = Context.mk_fresh_var ctx1 in
      let x1 = Dsl.v i1 in
      let ctx3 = ctx2 |> Context.add_eqn t1 (IList x1) in
      (IList x1, ctx3)

  with
  | Type_error msg -> ty_err (msg ^ "\nin expression " ^ string_of_expr e)

(** Module for type variable substitution *)
module Sub = Map.Make (Int)
type sub = ityp Sub.t

(** Module for set of integers *)
module IntSet = Set.Make (Int)

(** Find free type variables in a given type *)
let rec free_vars = function
  | IVar i -> IntSet.singleton i
  | IInt -> IntSet.empty
  | IList t -> free_vars t
  | IFun (t1, t2) -> IntSet.union (free_vars t1) (free_vars t2)

(** Apply the type substitution to the given type. *)
let rec sub_ityp sub = function
  | IInt -> IInt
  | IVar i -> begin match Sub.find_opt i sub with
      | None -> IVar i
      | Some t -> t
    end
  | IList t' -> IList (sub_ityp sub t')
  | IFun (t1, t2) -> IFun (sub_ityp sub t1, sub_ityp sub t2)

(** Apply the type substitution to each constraint in the given list of
   constraints. *)
let sub_cons sub cons =
  List.map (fun (t1, t2) -> (sub_ityp sub t1, sub_ityp sub t2)) cons

(** Unification algorithm that computes a type substitution that solves the
   constraints, or raises a Type_error if there is no solution. *)
let rec unify_helper (cons : (ityp * ityp) list) : sub = begin 
  match cons with
  | [] -> Sub.empty
  | h :: cons' -> begin match h with 
    | (t1, t2) when t1 = t2 -> unify_helper cons' 
    | (IVar n1, t2) when not (IntSet.mem n1 (free_vars t2)) -> 
      let sub = Sub.singleton n1 t2 in 
      Sub.add n1 t2 (unify_helper (sub_cons sub cons'))
    | (t1, IVar n2) when not (IntSet.mem n2 (free_vars t1)) -> 
      let sub = Sub.singleton n2 t1 in 
      Sub.add n2 t1 (unify_helper (sub_cons sub cons'))

    | (IFun(t11,t12), IFun(t21,t22)) -> unify_helper ((t11, t21) :: (t12, t22) :: cons')
    | (IList t1, IList t2) -> unify_helper ((t1, t2) :: cons')
    | _ -> ty_err "unify fail"
  end
end

let unify cons =
  let sub0 = unify_helper cons in
  let bound_vars = List.map fst (Sub.bindings sub0) in
  (* Replaces all type variables in the inferred type with concrete types,
     allowing only "free" type variables. This works by substituting type
     variables until a fixed point is reached (which is guaranteed since the
     occurs check ensures that the solution graph is a DAG). *)
  let rec apply_sub sub' =
    let ty_vars = List.fold_left IntSet.union IntSet.empty
        (List.map (fun (_, t) -> free_vars t) (Sub.bindings sub'))
    in
    if IntSet.is_empty (IntSet.inter ty_vars (IntSet.of_list bound_vars))
    then sub'
    else apply_sub (Sub.map (sub_ityp sub') sub')
  in
  apply_sub sub0

(** Hindley-Milner type inference *)
let type_infer (e : expr) : ityp * sub =
  let (ty, ctx) = gen_cons Context.empty e in
  
  (* Format.printf "Generated constraints:\n%!";
  List.iter (fun (t1, t2) -> Format.printf "%s = %s\n%!" (string_of_ityp t1) (string_of_ityp t2))
    (Context.cons ctx) ; *)
 
  let sub = unify (Context.cons ctx) in
  (*
  Format.printf "Computed substitution:\n%!";
  List.iter (fun (i, t) -> Format.printf "%d => %s\n%!" i (string_of_ityp t))
    (Sub.bindings sub) ;
  *)
  (sub_ityp sub ty, sub)
