open Ast

type ctx = (identifier * ol_value) list

and ol_value =
  | OLInt of int
  | OLBool of bool
  | OLString of string
  | OLUnit
  | OLTuple of ol_value list
  | OLUser of identifier * ol_value list
  | OLClosure of param list * ctx * expr * string option
  | OLBuiltin of (ol_value list -> ol_value)

let rec string_of_value = function
| OLInt i -> string_of_int i
| OLBool b -> string_of_bool b
| OLString s -> "\"" ^ s ^ "\""
| OLUnit -> "()"
| OLTuple vals -> "(" ^ String.concat ", " (List.map string_of_value vals) ^ ")"
| OLUser (id, vals) -> id ^ "(" ^ String.concat ", " (List.map string_of_value vals) ^ ")"
| OLClosure _ -> "<closure>"
| OLBuiltin _ -> "<builtin>"

let rec eval_expr (ctx: ctx) (expr: expr) : ol_value = 
  match expr with
  | IntExpr i -> OLInt i
  | BoolExpr b -> OLBool b
  | StringExpr s -> OLString s
  | Unit -> OLUnit
  | Identifier id -> 
    (match List.assoc_opt id ctx with
    | Some v -> v
    | None -> failwith ("Unbound identifier: " ^ id))
  | Tuple es -> OLTuple (List.map (eval_expr ctx) es)
  | UnaryOp (op, e) ->
    let v = eval_expr ctx e in
    (match op, v with
    | Neg, OLInt i -> OLInt (-i)
    | Not, OLBool b -> OLBool (not b)
    | _ -> failwith "Unary operation: Invalid type or operation")  
  | BinaryOp (op, e1, e2) ->
    let v1 = eval_expr ctx e1 in
    let v2 = eval_expr ctx e2 in
    (match op, v1, v2 with
    | Add, OLInt i1, OLInt i2 -> OLInt (i1 + i2)
    | Sub, OLInt i1, OLInt i2 -> OLInt (i1 - i2)
    | Mul, OLInt i1, OLInt i2 -> OLInt (i1 * i2)
    | Div, OLInt i1, OLInt i2 -> if i2 != 0 then OLInt (i1 / i2) else failwith "Division by zero"
    | Lt, OLInt i1, OLInt i2 -> OLBool (i1 < i2)
    | Eq, OLInt i1, OLInt i2 -> OLBool (i1 = i2)
    | Concat, OLString s1, OLString s2 -> OLString (s1 ^ s2)
    | And, OLBool b1, OLBool b2 -> OLBool (b1 && b2)
    | Or, OLBool b1, OLBool b2 -> OLBool (b1 || b2)
    | _ -> failwith "Binary operation: Invalid type or operation")
  | Let (id, params, _, e1, e2) ->
    let value_of_e1 = 
      match params with
      | [] -> eval_expr ctx e1
      | _ -> OLClosure (params, ctx, e1, None) 
    in
    let new_ctx = (id, value_of_e1) :: ctx in
    eval_expr new_ctx e2
  | LetRec (id, params, _, e1, e2) ->
    let rec_placeholder = OLClosure (params, ctx, e1, Some id) in
    let ctx_with_rec = (id, rec_placeholder) :: ctx in
    let function_body = eval_expr ctx_with_rec e1 in
    let ctx_with_function = (id, function_body) :: ctx in
    eval_expr ctx_with_function e2
  | If (e1, e2, e3) ->
    let cond = eval_expr ctx e1 in
    (match cond with
    | OLBool true -> eval_expr ctx e2
    | OLBool false -> eval_expr ctx e3
    | _ -> failwith "If condition: Expected a boolean")
  | Application (e1, e2) ->
    let v1 = eval_expr ctx e1 in
    let v2 = eval_expr ctx e2 in
    (match v1 with
    | OLBuiltin func -> func [v2]
    | OLClosure (params, closure_ctx, body, rec_id_opt) ->
        (match params with
        | [] -> failwith "Application: No parameters for function"
        | param::rest_params ->
            let new_ctx = (match param with
                            | SimpleParam id -> (id, v2)
                            | TypedParam (id, _) -> (id, v2)) :: closure_ctx in
            if rest_params = [] then
              (match rec_id_opt with
                | Some rec_id -> eval_expr ((rec_id, v1) :: new_ctx) body
                | None -> eval_expr new_ctx body)
            else OLClosure (rest_params, new_ctx, body, rec_id_opt))
    | OLUser(_, _) -> v1
    | _ -> failwith "Application: Function expected")
  | MatchExpr (e, branches) ->
    let value_to_match = eval_expr ctx e in
    (match value_to_match with
    | OLUser (constr, vals) ->
      let rec match_branch = function
        | [] -> failwith "Match failed: no branches matched"
        | MatchBranch (branch_constr, vars, expr) :: rest ->
          if branch_constr = constr then
            let new_ctx = List.combine vars vals @ ctx in
            eval_expr new_ctx expr
          else
            match_branch rest
      in
      match_branch branches
    | _ -> failwith "Match expression: Expected user-defined type value")
  | FunExpr (params, opt_typ, e) ->
    let closure_ctx = List.fold_left (fun acc_ctx param ->
        let param_name = 
          match param with
          | SimpleParam id -> id
          | TypedParam (id, _) -> id
        in
        (param_name, OLUnit) :: acc_ctx
      ) ctx params in
      (* Attemts to find the name of the recursive let binding *)
      let rec_name = 
        match opt_typ with
        | Some (IdType id) when List.exists (fun (binding_id, _) -> id = binding_id) ctx -> Some id
        | _ -> None
      in
      OLClosure (params, closure_ctx, e, rec_name)

let rec eval_binding (ctx: ctx) (binding: binding) : ctx =
  match binding with
  | ValueBinding(id, params, _, expr) ->
    if List.length params = 0 then
      let value = eval_expr ctx expr in
      (id, value) :: ctx
    else
      let closure = OLClosure (params, ctx, expr, None) in
      (id, closure) :: ctx
  | RecursiveBinding(id, params, _, expr) ->
    let rec_closure = OLClosure (params, ctx, expr, Some id) in
    let new_ctx = (id, rec_closure) :: ctx in
    new_ctx
  | TypeBinding(_, constructors) -> 
    let new_constructors = List.map (fun (Constructor (cons_id, cons_type_opt)) ->
      match cons_type_opt with
      | Some _ -> (cons_id, OLClosure ([], ctx, Identifier cons_id, None)) (* Constructors with arguments *)
      | None -> (cons_id, OLUser (cons_id, [])) (* Constructors without arguments *)
    ) constructors in
    new_constructors @ ctx

let rec eval_program (ctx: ctx) (bindings: binding list) : ctx =
  match bindings with
  | [] -> ctx
  | binding::rest ->
    let ctx' = eval_binding ctx binding in 
    eval_program ctx' rest

let interpret (ast: program) : ctx =
  let initial_ctx = [
    ("int_of_string", OLBuiltin (fun args ->
      match args with
      | [OLString s] -> (try OLInt (int_of_string s) with _ -> failwith "int_of_string failure")
      | _ -> failwith "int_of_string expects one string argument"
    ));
    ("string_of_int", OLBuiltin (fun args ->
      match args with
      | [OLInt i] -> OLString (string_of_int i)
      | _ -> failwith "string_of_int expects one integer argument"
    ));
    ("string_of_bool", OLBuiltin (fun args ->
      match args with
      | [OLBool b] -> OLString (string_of_bool b)
      | _ -> failwith "string_of_bool expects one boolean argument"
    ));
    ("print_string", OLBuiltin (fun args ->
      match args with
      | [OLString s] -> print_endline s; OLUnit
      | _ -> failwith "print_string expects one string argument"
    ))
  ] in
  eval_program initial_ctx ast