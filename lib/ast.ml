type identifier = string
[@@deriving show]

type program = binding list
[@@deriving show]

and binding =
  | ValueBinding of identifier * param list * typ option * expr
  | RecursiveBinding of identifier * param list * typ option * expr
  | TypeBinding of identifier * constructor list
[@@deriving show]

and constructor =
  | Constructor of identifier * typ option
[@@deriving show]

and param =
  | SimpleParam of identifier
  | TypedParam of identifier * typ
[@@deriving show]

and expr =
  | Let of identifier * param list * typ option * expr * expr
  | LetRec of identifier * param list * typ option * expr * expr
  | If of expr * expr * expr
  | FunExpr of param list * typ option * expr
  | Application of expr * expr
  | Tuple of expr list
  | BinaryOp of binop * expr * expr
  | UnaryOp of unop * expr
  | Paren of expr
  | IntExpr of int
  | BoolExpr of bool
  | StringExpr of string
  | Identifier of identifier
  | Unit
  | MatchExpr of expr * match_branch list
[@@deriving show]

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
[@@deriving show]

and unop =
  | Not
  | Neg
[@@deriving show]

and typ =
  | FunType of typ * typ
  | TupleType of typ list
  | IntType
  | BoolType
  | StringType
  | UnitType
  | IdType of identifier
[@@deriving show]

and match_branch =
  | MatchBranch of identifier * string list * expr
[@@deriving show]