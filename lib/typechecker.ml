open Ast
open Dump

exception UnificationError of string

(* Helper for finding free variables*)
let union lst1 lst2 =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) lst1 lst2

type typecheck_result =
  | Ok of (identifier * typ) list
  | Error of string

type substitution = (int * typ) list 

type constraint_type = typ * typ

type constraints = constraint_type list

type environment = (string * typ) list

let rec free_vars (t: typ) : int list =
  match t with
  | VarTy var_id -> [var_id]
  | FunType(arg, ret) -> union (free_vars arg) (free_vars ret)
  | TupleType types -> List.fold_left (fun acc ty -> union acc (free_vars ty)) [] types
  | IntType | BoolType | StringType | UnitType | IdType _ -> []
  | Forall(var_id, ty) -> List.filter (fun id -> id != var_id) (free_vars ty)

let free_vars_env (env: environment) : int list =
  List.fold_left (fun acc (_, typ) -> union acc (free_vars typ)) [] env  
  
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
  | Forall(var_id, ty) ->
    if List.mem_assoc var_id subst then t
    else Forall(var_id, apply_substitution subst ty)  

let fresh_type_var =
  let counter = ref 0 in
  fun () -> incr counter; VarTy (!counter)
  
let generalize (env: environment) (t: typ) : typ =
  let free_in_env = free_vars_env env in
  let free_in_type = free_vars t in
  let to_quantify = List.filter (fun v -> not (List.mem v free_in_env)) free_in_type in
  List.fold_right (fun var acc -> Forall(var, acc)) to_quantify t

let rec instantiate (t: typ) : typ =
  match t with
  | Forall(var_id, ty) ->
    let fresh_var = fresh_type_var () in
    let subst = [(var_id, fresh_var)] in
    instantiate (apply_substitution subst ty)
  | _ -> t  

let rec type_infer (env: environment) (expr: expr) : (typ * constraints) =
  let result = 
    match expr with
    | IntExpr _ -> (IntType, [])
    | BoolExpr _ -> (BoolType, [])
    | StringExpr _ -> (StringType, [])
    | Unit -> (UnitType, [])
    | Identifier id ->
      (match List.assoc_opt id env with
        | Some typ -> (typ, [])
        | None -> failwith ("Unbound identifier or type: " ^ id))
    | Let (id, params, opt_typ, e1, e2) ->
      let new_env, _ = extend_env_with_params env params in
      let (t1, c1) = type_infer new_env e1 in
      (match unify_constraints c1 with
      | Error msg -> failwith msg
      | Ok subst ->
        let updated_env = apply_substitution_to_env subst env in
        let updated_t1 = apply_substitution subst t1 in
        let fun_type = match params with
          | [] -> updated_t1
          | _ -> build_fun_type params updated_t1
        in
        let generalized_fun_type = generalize updated_env fun_type in
        let env' = (id, generalized_fun_type) :: env in
        let (t2, c2) = type_infer env' e2 in
        let constraints = match opt_typ with
          | Some opt_typ_ann -> (opt_typ_ann, generalized_fun_type) :: c2
          | None -> c2
      in
      (t2, constraints))
    | LetRec (id, params, opt_typ, e1, e2) ->
      let new_env, _ = extend_env_with_params env params in
      let fun_type = match opt_typ with
        | Some typ -> typ
        | None -> build_fun_type params (fresh_type_var ())
      in
      let env_with_fun = (id, fun_type) :: new_env in
      let (_, c1) = type_infer env_with_fun e1 in
      (match unify_constraints c1 with 
      | Error msg -> failwith msg
      | Ok subst ->
          let updated_env = apply_substitution_to_env subst env in 
          let generalized_fun_type = generalize updated_env fun_type in
          let env' = (id, generalized_fun_type) :: env in
          let (t2, c2) = type_infer env' e2 in 
          let constraints = match opt_typ with 
            | Some opt_type_ann -> (opt_type_ann, generalized_fun_type) :: c2
            | None -> c2
      in
      (t2, constraints))  
    | If (e1, e2, e3) ->
      let (t1, c1) = type_infer env e1 in
      let (t2, c2) = type_infer env e2 in
      let (t3, c3) = type_infer env e3 in
      let constraints = c1 @ c2 @ c3 @ [(t1, BoolType); (t2, t3)] in
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
      let env', _ = extend_env_with_params env params in
      let (t, c) = type_infer env' e in
      let fun_type = build_fun_type (List.rev params) (match opt_typ with
                                                          | Some typ -> typ
                                                          | None -> t) in
      (fun_type, c)
    | Application (e1, e2) ->
      let (t1, c1) = type_infer env e1 in
      let (t2, c2) = type_infer env e2 in
      let result_type = fresh_type_var () in
  
      let t1_instantiated = instantiate t1 in
      let c = (t1_instantiated, FunType(t2, result_type)) :: c1 @ c2 in
      (result_type, c)    
    | Tuple es ->
      let ts_cs = List.map (type_infer env) es in
      let ts = List.map fst ts_cs in
      let cs = List.concat (List.map snd ts_cs) in
      (TupleType ts, cs)    
    | MatchExpr (e, branches) ->
      let (t, c) = type_infer env e in
      (* Typecheck each branch type of the match expr, make sure all have the same type. *)
      let branch_results = List.map (infer_match_branch env t) branches in
      let branch_types = List.map fst branch_results in
      let branch_constraints = List.concat (List.map snd branch_results) in
  
      let all_branches_same_type = 
        match branch_types with
        | [] -> []
        | hd :: tl -> List.map (fun bt -> (hd, bt)) tl in
  
      match branch_types with
      | hd :: _ -> (hd, c @ branch_constraints @ all_branches_same_type)
      | [] -> failwith "Match expression must have at least one branch"
    in
    result

and infer_binop_type op t1 t2 =
  match op with
  | Add | Sub | Mul | Div | Mod -> (IntType, [(t1, IntType); (t2, IntType)])
  | Lt | Eq -> (BoolType, [(t1, t2)])
  | Concat -> (StringType, [(t1, StringType); (t2, StringType)])
  | And | Or -> (BoolType, [(t1, BoolType); (t2, BoolType)])
    
and infer_unop_type op t =
  match op with
  | Not -> (BoolType, [(t, BoolType)])
  | Neg -> (IntType, [(t, IntType)])
  
and infer_match_branch env matched_type (MatchBranch (cons_id, vars, e)) =
  match List.assoc_opt cons_id env with
  | Some cons_type ->
    let extended_env = List.fold_left (fun acc var -> (var, cons_type) :: acc) env vars in
    let (t, c) = type_infer extended_env e in
    (t, c @ [(matched_type, cons_type)])
  | None ->
    failwith ("Unknown constructor: " ^ cons_id)  

and extend_env_with_params env params =
  List.fold_left (fun (acc_env, acc_types) param ->
    match param with
    | SimpleParam id ->
        let t = fresh_type_var () in
        ((id, t) :: acc_env, t :: acc_types)
    | TypedParam (id, typ) ->
        ((id, typ) :: acc_env, typ :: acc_types)
  ) (env, []) params

and unify_constraints (constraints: constraints) : (substitution, string) result =
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

and build_fun_type params return_type =
  match params with
  | [] -> return_type
  | p :: ps ->
      let param_type = match p with
        | SimpleParam(_) -> fresh_type_var ()
        | TypedParam(_, typ) -> typ
      in
      FunType(param_type, build_fun_type ps return_type)

and apply_substitution_to_env (subst: substitution) (env: environment) : environment =
  List.map (fun (id, typ) -> (id, apply_substitution subst typ)) env

let process_binding env (binding: binding) : (environment * constraints, string) result =
  match binding with
  | ValueBinding(id, params, opt_typ, expr) ->
    let new_env, _ = extend_env_with_params env params in
    let (t1, c1) = type_infer new_env expr in
    let c1_with_opt_typ = match opt_typ with
      | Some annotated_type -> (annotated_type, t1) :: c1
      | None -> c1 in
    (match unify_constraints c1_with_opt_typ with
    | Error msg -> Error msg
    | Ok subst ->
      let generalized_t1 = generalize (apply_substitution_to_env subst env) t1 in
      let env' = (id, generalized_t1) :: env in
      Ok (env', c1_with_opt_typ))
  | RecursiveBinding(id, params, _, expr) ->
    let fun_type_var = fresh_type_var () in
    let recursive_env = (id, fun_type_var) :: env in
    let new_env, _ = extend_env_with_params recursive_env params in
    let (t1, c1) = type_infer new_env expr in
    (match unify_constraints c1 with
    | Error msg -> Error msg
    | Ok subst ->
      let generalized_t1 = generalize (apply_substitution_to_env subst env) t1 in
      let env' = (id, generalized_t1) :: env in
      Ok (env', c1 @ [(fun_type_var, t1)]))
  | TypeBinding(type_id, constructors) ->
    let env' = List.fold_left (fun acc_env (Constructor (cons_id, cons_type_opt)) ->
      let cons_type = match cons_type_opt with
        | Some ct -> ct
        | None -> IdType type_id
      in
      (cons_id, cons_type) :: acc_env
    ) env constructors in
    Ok (env', [])

let rec process_program env constraints = function
| [] -> 
  begin match unify_constraints constraints with
  | Error msg -> Error msg
  | Ok subst ->
    let final_env = apply_substitution_to_env subst env in
    Ok final_env
  end
| binding :: rest ->
  begin match process_binding env binding with
  | Error msg -> Error msg
  | Ok (env', c) -> process_program env' (constraints @ c) rest
  end

let type_check (ast: program) : typecheck_result =
  match process_program [] [] ast with
  | Error msg -> Error msg
  | Ok env -> Ok env
