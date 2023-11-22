open Ast
open Dump

exception UnificationError of string

(* Helper for finding free variables*)
let union lst1 lst2 =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) lst1 lst2

type typecheck_result =
  | Ok of (identifier * typ) list
  | Error of string

type tytyp = 
| Mono of typ
| Forall of int * tytyp

type substitution = (int * typ) list 

type constraint_type = typ * typ

type constraints = constraint_type list

type environment = (string * tytyp) list

let rec free_vars (t: typ) : int list =
  match t with
  | VarTy var_id -> [var_id]
  | FunType(arg, ret) -> union (free_vars arg) (free_vars ret)
  | TupleType types -> List.fold_left (fun acc ty -> union acc (free_vars ty)) [] types
  | IntType | BoolType | StringType | UnitType | IdType _ -> []

let rec free_vars_tytyp (t: tytyp) : int list =
  match t with
  | Mono typ -> free_vars typ
  | Forall (var_id, ty) -> List.filter (fun id -> id != var_id) (free_vars_tytyp ty)

let free_vars_env (env: environment) : int list =
  let rec free_vars_in_tytyp acc = function
    | Mono typ -> union acc (free_vars typ)
    | Forall (var_id, tytyp) -> free_vars_in_tytyp (List.filter (fun id -> id != var_id) acc) tytyp
  in
  List.fold_left (fun acc (_, tytyp) -> free_vars_in_tytyp acc tytyp) [] env
  
let rec apply_substitution (subst: substitution) (t: typ) : typ =
  match t with
  | VarTy var_id ->
    (match List.assoc_opt var_id subst with
      | Some ty -> apply_substitution subst ty
      | None -> t)
  | FunType(arg, ret) ->
    FunType(apply_substitution subst arg, apply_substitution subst ret)
  | TupleType types ->
    TupleType(List.map (apply_substitution subst) types)
  | IntType | BoolType | StringType | UnitType | IdType _ ->
    t

let fresh_type_var =
  let counter = ref 0 in
  fun () -> incr counter; VarTy (!counter)

let instantiate (t: tytyp) : typ =
  let rec instantiate' subst t =
    match t with
    | Mono typ -> (apply_substitution subst typ, subst)
    | Forall (var_id, ty) ->
      let fresh_var = fresh_type_var () in
      let new_subst = (var_id, fresh_var) :: subst in
      instantiate' new_subst ty
  and apply_substitution subst typ =
    match typ with
    | VarTy var_id -> (try List.assoc var_id subst with Not_found -> typ)
    | FunType(arg, ret) -> FunType(apply_substitution subst arg, apply_substitution subst ret)
    | TupleType types -> TupleType(List.map (apply_substitution subst) types)
    | IntType | BoolType | StringType | UnitType | IdType _ -> typ
  in
  fst (instantiate' [] t)
  
let generalize (env: environment) (t: typ) : tytyp =
  let free_in_env = free_vars_env env in
  let free_in_type = free_vars t in
  let to_quantify = List.filter (fun v -> not (List.mem v free_in_env)) free_in_type in
  List.fold_right (fun var acc -> Forall(var, acc)) to_quantify (Mono t)

let rec type_infer (env: environment) (expr: expr) : (tytyp * constraints) =
  let result = 
    match expr with
    | IntExpr _ -> (Mono IntType, [])
    | BoolExpr _ -> (Mono BoolType, [])
    | StringExpr _ -> (Mono StringType, [])
    | Unit -> (Mono UnitType, [])
    | Identifier id ->
      (match List.assoc_opt id env with
        | Some typ -> (typ, [])
        | None -> failwith ("Unbound identifier or type: " ^ id))
    | Let (id, params, opt_typ, e1, e2) ->
      let new_env, _ = extend_env_with_params env params in
      (* First, type-check e1*)
      let (t1, c1) = type_infer new_env e1 in
      let env' = (id, t1) :: env in
      (* Next, extend the environment with the type of e2*)
      let (t2, c2) = type_infer env' e2 in
      let t1_typ = match t1 with
      | Mono typ -> typ
      | Forall _ -> failwith "Expected Mono type in Let binding" in
      let constraints = match opt_typ with
      | Some opt_typ_ann -> ((opt_typ_ann, t1_typ) :: c1) @ c2
      | None -> c1 @ c2 in
      (t2, constraints)
    | LetRec (id, params, opt_typ, e1, e2) ->
      let new_env, param_types = extend_env_with_params env params in
      (* First, extend the environment with the function type itself *)
      (* to refer to it later in the body. *)
      let fun_type = match opt_typ with
      | Some typ -> Mono typ
      | None -> Mono (FunType(TupleType param_types, fresh_type_var ())) in
      let env' = (id, fun_type) :: new_env in
      let (t1, c1) = type_infer env' e1 in
      let t1_typ = match t1 with
      | Mono typ -> typ
      | Forall _ -> failwith "Expected Mono type in LetRec binding" in
      (* Next, extend the environment with the type of e2*)
      let (t2, c2) = type_infer ((id, fun_type) :: env) e2 in
      let fun_type_typ = match fun_type with
      | Mono typ -> typ
      | Forall _ -> failwith "Expected Mono type for function in LetRec" in
      let constraints = ((fun_type_typ, t1_typ) :: c1) @ c2 in
      (t2, constraints)
    | If (e1, e2, e3) ->
      let (t1, c1) = type_infer env e1 in
      let (t2, c2) = type_infer env e2 in
      let (t3, c3) = type_infer env e3 in
      let t1_typ = match t1 with
        | Mono typ -> typ
        | Forall _ -> failwith "Expected Mono type in If condition" in
      let t2_typ = match t2 with
        | Mono typ -> typ
        | Forall _ -> failwith "Expected Mono type in If then branch" in
      let t3_typ = match t3 with
        | Mono typ -> typ
        | Forall _ -> failwith "Expected Mono type in If else branch" in
      let constraints = c1 @ c2 @ c3 @ [(t1_typ, BoolType); (t2_typ, t3_typ)] in
      (t2, constraints)
    | BinaryOp (op, e1, e2) ->
      let (t1, c1) = type_infer env e1 in
      let (t2, c2) = type_infer env e2 in
      let (t, c) = infer_binop_type op t1 t2 in
      (t, c1 @ c2 @ c)
    | UnaryOp (op, e) ->
      let (t, _) = type_infer env e in
      infer_unop_type op t
    | FunExpr (params, opt_typ, e) ->
      let rec build_fun_type params return_type =
        match params with
        | [] -> return_type
        | p :: ps ->
          let param_type = match p with
            | SimpleParam(_) -> fresh_type_var ()
            | TypedParam(_, typ) -> typ
          in
          FunType(param_type, build_fun_type ps return_type)
      in

      let env', _ = extend_env_with_params env params in
      let (t, c) = type_infer env' e in
      let t_typ = match t with
        | Mono typ -> typ
        | Forall _ -> failwith "Unexpected polymorphic type in function expression" in
      let fun_type = build_fun_type (List.rev params) (match opt_typ with
                                                          | Some typ -> typ
                                                          | None -> t_typ) in
      (Mono fun_type, c)
    | Application (e1, e2) ->
      let (t1, c1) = type_infer env e1 in
      let (t2, c2) = type_infer env e2 in
      let result_type = fresh_type_var () in
      
      (* Logging for debugging *)
      Printf.printf "Application: Function Expression Type: %s\n" (string_of_typ (match t1 with Mono typ -> typ | _ -> failwith "Complex type in application"));
      Printf.printf "Application: Argument Expression Type: %s\n" (string_of_typ (match t2 with Mono typ -> typ | _ -> failwith "Complex type in argument"));
      
      let t1_typ = match t1 with
        | Mono typ -> typ
        | Forall _ -> failwith "Unexpected polymorphic type in application" in
      let t2_typ = match t2 with
        | Mono typ -> typ
        | Forall _ -> failwith "Unexpected polymorphic type in application" in
      
      (* Additional logging to understand the expected function type *)
      Printf.printf "Expected Function Type in Application: %s -> %s\n" (string_of_typ t2_typ) (string_of_typ result_type);
  
      let c = (t1_typ, FunType(t2_typ, result_type)) :: c1 @ c2 in
      
      (* Logging the generated constraint for function application *)
      Printf.printf "Generated Constraint for Function Application: %s = %s -> %s\n" (string_of_typ t1_typ) (string_of_typ t2_typ) (string_of_typ result_type);
  
      (Mono result_type, c)
    | Tuple es ->
      let ts_cs = List.map (type_infer env) es in
      let ts = List.map (fun (t, _) -> match t with
                                        | Mono typ -> typ
                                        | Forall _ -> failwith "Unexpected polymorphic type in tuple") ts_cs in
      let cs = List.concat (List.map snd ts_cs) in
      (Mono (TupleType ts), cs)
    | MatchExpr (e, branches) ->
      let (t, c) = type_infer env e in
      let t_typ = match t with
        | Mono typ -> typ
        | Forall _ -> failwith "Unexpected polymorphic type in match expression" in
      (* Typecheck each branch type of the match expr, make sure all have the same type. *)
      let branch_results = List.map (infer_match_branch env t_typ) branches in
      let branch_types = List.map fst branch_results in
      let branch_constraints = List.concat (List.map snd branch_results) in
      let all_branches_same_type = List.map (fun bt -> (List.hd branch_types, bt)) (List.tl branch_types) in
      (Mono (List.hd branch_types), c @ branch_constraints @ all_branches_same_type)
    in
    let inferred_type_string = 
      match fst result with
      | Mono typ -> string_of_typ typ
      | Forall _ -> "Polymorphic type (Forall)"
    in
    Printf.printf "Type inferred: %s\n" inferred_type_string; 
    result

and infer_binop_type op t1 t2 =
  let t1_typ = match t1 with
    | Mono typ -> typ
    | Forall _ -> failwith "Unexpected polymorphic type in binary operation" in
  let t2_typ = match t2 with
    | Mono typ -> typ
    | Forall _ -> failwith "Unexpected polymorphic type in binary operation" in
  match op with
  | Add | Sub | Mul | Div | Mod -> (Mono IntType, [(t1_typ, IntType); (t2_typ, IntType)])
  | Lt | Eq -> (Mono BoolType, [(t1_typ, t2_typ)])
  | Concat -> (Mono StringType, [(t1_typ, StringType); (t2_typ, StringType)])
  | And | Or -> (Mono BoolType, [(t1_typ, BoolType); (t2_typ, BoolType)])
    
and infer_unop_type op t =
  let t_typ = match t with
    | Mono typ -> typ
    | Forall _ -> failwith "Unexpected polymorphic type in unary operation" in
  match op with
  | Not -> (Mono BoolType, [(t_typ, BoolType)])
  | Neg -> (Mono IntType, [(t_typ, IntType)])

and infer_match_branch env matched_type (MatchBranch (_, vars, e)) =
  let extended_env = List.fold_left (fun acc var -> (var, Mono matched_type) :: acc) env vars in
  let (t, c) = type_infer extended_env e in
  let t_typ = match t with
    | Mono typ -> typ
    | Forall _ -> failwith "Unexpected polymorphic type in match branch" in
  (t_typ, c)

and extend_env_with_params env params =
  List.fold_left (fun (acc_env, acc_types) param ->
    match param with
    | SimpleParam id ->
        let t = fresh_type_var () in
        ((id, Mono t) :: acc_env, t :: acc_types)
    | TypedParam (id, typ) ->
        ((id, Mono typ) :: acc_env, typ :: acc_types)
  ) (env, []) params

let rec unify_constraints (constraints: constraints) : (substitution, string) result =
  match constraints with
  | [] -> Ok []
  | (t1, t2) :: rest ->
    match unify_types t1 t2 with
    | Error msg -> Error msg
    | Ok subst1 ->
      let rest_substituted = List.map (fun (a, b) -> (apply_substitution subst1 a, apply_substitution subst1 b)) rest in
      match unify_constraints rest_substituted with
      | Error msg -> Error msg
      | Ok subst2 -> Ok (subst1 @ subst2)

and unify_types t1 t2 : (substitution, string) result =
  Printf.printf "Unifying types: %s and %s\n" (string_of_typ t1) (string_of_typ t2);
  if t1 = t2 then Ok []
  else match (t1, t2) with
  | (VarTy var_id, _) -> 
      if occurs_check var_id t2 then 
        Error "Occurs check failed" 
      else 
        Ok [(var_id, t2)]
  | (_, VarTy var_id) -> 
      if occurs_check var_id t1 then 
        Error "Occurs check failed" 
      else 
        Ok [(var_id, t1)]
  | (FunType(arg1, ret1), FunType(arg2, ret2)) -> 
      unify_constraints [(arg1, arg2); (ret1, ret2)]
  | (TupleType types1, TupleType types2) when List.length types1 = List.length types2 ->
      unify_constraints (List.combine types1 types2)
  | (IdType id1, IdType id2) when id1 = id2 -> Ok []
  | _ -> Error (Printf.sprintf "Unification failed for types: %s and %s" (string_of_typ t1) (string_of_typ t2))


and occurs_check (var_id: int) (t: typ) : bool =
  match t with
  | VarTy id -> id = var_id
  | FunType(arg, ret) -> occurs_check var_id arg || occurs_check var_id ret
  | TupleType types -> List.exists (occurs_check var_id) types
  | _ -> false

let rec apply_substitution_to_env (subst: substitution) (env: environment) : environment =
  List.map (fun (id, tytyp) ->
    let substituted_tytyp =
      match tytyp with
      | Mono typ -> Mono (apply_substitution subst typ)
      | Forall (var_id, ty) ->
          (* Avoid substituting the quantified variable *)
          if List.mem_assoc var_id subst then tytyp
          else Forall (var_id, apply_substitution_to_tytyp subst ty)
    in
    (id, substituted_tytyp)
  ) env

and apply_substitution_to_tytyp subst tytyp =
  match tytyp with
  | Mono typ -> Mono (apply_substitution subst typ)
  | Forall (var_id, ty) ->
      if List.mem_assoc var_id subst then tytyp
      else Forall (var_id, apply_substitution_to_tytyp subst ty)

let process_binding env (binding: binding) : environment * constraints =
  match binding with
  | ValueBinding(id, params, opt_typ, expr) ->
    let new_env, _ = extend_env_with_params env params in
    let (t1, c1) = type_infer new_env expr in
    let t1_typ = match t1 with
      | Mono typ -> typ
      | Forall _ -> failwith "Unexpected polymorphic type in ValueBinding" in
    let c1 = match opt_typ with
      | Some typ -> (typ, t1_typ) :: c1
      | None -> c1 in
    let env' = (id, t1) :: env in
    (env', c1)
  | RecursiveBinding(id, params, _, expr) ->
    let fun_type_var = fresh_type_var () in
    let recursive_env = (id, Mono fun_type_var) :: env in
    let new_env, _ = extend_env_with_params recursive_env params in
    let (t1, c1) = type_infer new_env expr in
    let t1_typ = match t1 with
      | Mono typ -> typ
      | Forall _ -> failwith "Unexpected polymorphic type in RecursiveBinding" in
    let c1_with_recursion = (fun_type_var, t1_typ) :: c1 in
    let env' = (id, t1) :: env in
    (env', c1_with_recursion)
  | TypeBinding(type_id, constructors) ->
    let env' = List.fold_left (fun acc_env (Constructor (cons_id, cons_type_opt)) ->
      match cons_type_opt with
      | Some cons_type ->
        (cons_id, Mono cons_type) :: acc_env
      | None ->
        (cons_id, Mono (IdType type_id)) :: acc_env
    ) env constructors in
    (env', [])
      
let rec process_program env constraints = function
| [] -> 
  begin match unify_constraints constraints with
  | Error msg -> Error msg
  | Ok subst ->
    let final_env = apply_substitution_to_env subst env in
    let final_env_converted = List.map (fun (id, tytyp) -> 
      match tytyp with
      | Mono typ -> (id, typ)
      | Forall _ -> failwith "Unexpected polymorphic type in final environment") final_env in
    Ok final_env_converted
  end
| binding :: rest ->
  let (env', c) = process_binding env binding in
  process_program env' (constraints @ c) rest

let type_check (ast: program) : typecheck_result =
  match process_program [] [] ast with
  | Error msg -> Error msg
  | Ok env -> Ok env
