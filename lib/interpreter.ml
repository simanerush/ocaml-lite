open Ast

type ctx = (identifier * ol_value) list

and ol_value =
  | OLInt of int
  | OLBool of bool
  | OLString of string
  | OLUnit
  | OLTuple of ol_value list
  | OLUser of identifier * ol_value list
  | OLCons of identifier (* Constructor that wasn't applied yet *)
  | OLClosure of identifier * ctx * expr * string option
  | OLBuiltin of (ol_value list -> ol_value)

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
  | Let (id, params, opt_typ, e1, e2) -> 
    (* Placeholder for let evaluation *)
    OLUnit  (* Replace with actual implementation *)
  | LetRec (id, params, opt_typ, e1, e2) -> 
    (* Placeholder for let rec evaluation *)
    OLUnit  (* Replace with actual implementation *)
  | If (e1, e2, e3) -> 
    (* Placeholder for if evaluation *)
    OLUnit  (* Replace with actual implementation *)
  | Application (e1, e2) ->
    let v1 = eval_expr ctx e1 in
    let v2 = eval_expr ctx e2 in
    (match v1 with
    | OLBuiltin func -> func [v2]
    | OLClosure (param, closure_ctx, body, rec_id_opt) ->
        let new_ctx = (param, v2) :: closure_ctx in
        (match rec_id_opt with
        | Some rec_id -> eval_expr ((rec_id, v1) :: new_ctx) body
        | None -> eval_expr new_ctx body)
    | _ -> failwith "Application: Function expected")
  | MatchExpr (e, branches) -> 
    (* Placeholder for match expression evaluation *)
    OLUnit  (* Replace with actual implementation *)
  | _ -> failwith "Expression type not supported yet"  

let rec eval_binding (ctx: ctx) (binding: binding) : ctx =
  match binding with
  | ValueBinding(id, params, opt_typ, expr) -> 
    let value = eval_expr ctx expr in
    (id, value) :: ctx
  | RecursiveBinding(id, params, opt_typ, expr) ->
    (* Placeholder for recursive binding evaluation *)
    ctx  (* Replace with actual implementation *)
  | TypeBinding(type_id, constructors) -> 
    (* Placeholder for type binding evaluation *)
    ctx  (* Replace with actual implementation *)  

and eval_program (ctx: ctx) (bindings: binding list) : ctx =
  match bindings with
  | [] -> []
  | binding:: rest ->
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
    ("print_string", OLBuiltin (fun args ->
      match args with
      | [OLString s] -> print_endline s; OLUnit
      | _ -> failwith "print_string expects one string argument"
    ))
  ] in
  eval_program initial_ctx ast