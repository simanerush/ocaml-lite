open Ast

let rec dump_ast (prog: program) : string =
  String.concat "\n" (List.map string_of_binding prog)

and string_of_binding = function
  | ValueBinding(id, params, typ, e) ->
      "let " ^ id ^ string_of_params params ^ string_of_typ_option typ ^ " = " ^ string_of_expr e
  | RecursiveBinding(id, params, typ, e) ->
      "let rec " ^ id ^ string_of_params params ^ string_of_typ_option typ ^ " = " ^ string_of_expr e
  | TypeBinding(id, constructors) ->
      "type " ^ id ^ " = " ^ String.concat " | " (List.map string_of_constructor constructors)

and string_of_constructor (Constructor(id, typ)) =
  id ^ (match typ with
        | None -> ""
        | Some t -> " of " ^ string_of_typ t)

and string_of_params params =
  String.concat " " (List.map string_of_param params)

and string_of_param = function
  | SimpleParam id -> id
  | TypedParam (id, t) -> "(" ^ id ^ " : " ^ string_of_typ t ^ ")"

and string_of_typ_option = function
  | None -> ""
  | Some t -> " : " ^ string_of_typ t

and string_of_typ = function
  | IntType -> "int"
  | BoolType -> "bool"
  | StringType -> "string"
  | UnitType -> "unit"
  | IdType id -> id
  | VarTy id -> "var" ^ string_of_int id
  | FunType(arg, ret) -> "(" ^ string_of_typ arg ^ " -> " ^ string_of_typ ret ^ ")"
  | TupleType types -> "(" ^ String.concat " * " (List.map string_of_typ types) ^ ")"

and string_of_expr = function
  | Let(id, params, typ, e1, e2) ->
      "let " ^ id ^ string_of_params params ^ string_of_typ_option typ ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | LetRec(id, params, typ, e1, e2) ->
      "let rec " ^ id ^ string_of_params params ^ string_of_typ_option typ ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | If(e1, e2, e3) ->
      "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | FunExpr(params, typ, e) ->
      "fun " ^ string_of_params params ^ string_of_typ_option typ ^ " => " ^ string_of_expr e
  | Application(e1, e2) ->
      string_of_expr e1 ^ " " ^ string_of_expr e2
  | Tuple(es) ->
      "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")"
  | BinaryOp(op, e1, e2) ->
      string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
  | UnaryOp(op, e) ->
      string_of_unop op ^ " " ^ string_of_expr e
  | IntExpr n ->
      string_of_int n
  | BoolExpr b ->
      string_of_bool b
  | StringExpr s ->
      "\"" ^ s ^ "\""
  | Identifier id ->
      id
  | Unit ->
      "()"
  | MatchExpr(e, branches) ->
      "match " ^ string_of_expr e ^ " with " ^ String.concat " | " (List.map string_of_match_branch branches)

and string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "mod"
  | Lt -> "<"
  | Eq -> "=="
  | Concat -> "^"
  | And -> "&&"
  | Or -> "||"

and string_of_unop = function
  | Not -> "not"
  | Neg -> "~"

and string_of_match_branch (MatchBranch(id, patterns, e)) =
  id ^ " " ^ String.concat " " patterns ^ " => " ^ string_of_expr e
