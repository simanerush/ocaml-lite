(* Identifiers *)
type identifier = string

(* Programs *)
type program = binding list

(* Bindings *)
and binding =
  | ValueBinding of identifier * param list * typ option * expr
  | RecursiveBinding of identifier * param list * typ option * expr
  | TypeBinding of identifier * constructor list

(* Constructors for type definitions *)
and constructor =
  | Constructor of identifier * typ option

(* Parameters *)
and param =
  | SimpleParam of identifier
  | TypedParam of identifier * typ

(* Expressions *)
and expr =
  | Let of identifier * param list * typ option * expr * expr
  | LetRec of identifier * param list * typ option * expr * expr
  | If of expr * expr * expr
  | Fun of param list * typ option * expr
  | Application of expr * expr
  | Tuple of expr list
  | BinaryOp of binop * expr * expr
  | UnaryOp of unop * expr
  | Paren of expr
  | Int of int
  | Bool of bool
  | String of string
  | Identifier of identifier
  | Unit
  | Match of expr * match_branch list

(* Binary operators *)
and binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Lt
  | Eq
  | Concat
  | And
  | Or

(* Unary operators *)
and unop =
  | Not
  | Neg

(* Types *)
and typ =
  | Arrow of typ * typ
  | ParenType of typ
  | TupleType of typ * typ
  | IntType
  | BoolType
  | StringType
  | UnitType
  | IdType of identifier

(* Match branches *)
and match_branch =
  | MatchBranch of identifier * pattern_var list * expr

(* Pattern variables *)
and pattern_var = 
  | SimpleVar of identifier
  | TupleVar of identifier list
