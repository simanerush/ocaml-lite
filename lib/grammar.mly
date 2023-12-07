%{
    open Ast
%}

%token Type "type"
%token Of "of"
%token Let "let"
%token Rec "rec"
%token In "in"
%token If "if"
%token Then "then"
%token Else "else"
%token Match "match"
%token With "with"
%token Fun "fun"
%token True "true"
%token False "false"
%token Mod "mod"
%token TInt "int"
%token TBool "bool"
%token TString "string"
%token TUnit "unit"
%token Eq "=="
%token Plus "+"
%token Minus "-"
%token Times "*"
%token Divide "/"
%token Lt "<"
%token Concat "^"
%token And "&&"
%token Or "||"
%token Not "not"
%token Negate "~"
%token DoubleSemicolon ";;"
%token Colon ":"
%token Arrow "->"
%token DoubleArrow "=>"
%token LParen "("
%token RParen ")"
%token Pipe "|"
%token Comma ","
%token <string> Id "id"
%token <int> Int "1"
%token <string> String "\"hello\""
%token EOF

%start <program> prog

%type <binding> binding
%type <constructor> constructor
%type <param> param
%type <expr> expr, expr_appl, expr_other
%type <match_branch> match_branch
%type <string list> pattern_vars
%type <typ> typ
%type <binop> binop
%type <unop> unop

%nonassoc In
%nonassoc Else
%right Arrow
%right DoubleArrow

%left Or
%left And
%left Lt, Eq
%left Plus, Minus, Concat
%left Times, Divide, Mod
%nonassoc Negate
%nonassoc Not

%%

let prog :=
    | EOF; { [] }
    | b = binding; DoubleSemicolon; p = prog; { b :: p }

let binding :=
    | Let; id = Id; params = param*; Colon?; typ = typ?; Eq; e = expr; { ValueBinding(id, params, typ, e) }
    | Let; Rec; id = Id; params = param*; Colon?; typ = typ?; Eq; e = expr; { RecursiveBinding(id, params, typ, e) }
    | Type; id = Id; Eq; Pipe?; t = separated_nonempty_list(Pipe, constructor); { TypeBinding(id, t) }

let constructor :=
    | id = Id; t = option(Of; typ); { Constructor(id, t) }

let typed_param :=
    | LParen; id = Id; Colon; t = typ; RParen; { TypedParam(id, t) }

let param :=
    | id = Id; { SimpleParam(id) }
    | tp = typed_param; { tp }

let expr :=
    | e = expr_non_appl; { e }

let expr_non_appl :=
    | e1 = expr_appl; op = binop; e2 = expr_non_appl; { BinaryOp(op, e1, e2) }
    | op = unop; e = expr_non_appl; { UnaryOp(op, e) }
    | Let; id = Id; params = param*; Colon?; typ = typ?; Eq; e = expr; In; e2 = expr; { Let(id, params, typ, e, e2) }
    | Fun; params = param+; t = option(Colon; typ); DoubleArrow; e = expr; { FunExpr(params, t, e) }
    | If; e = expr; Then; e2 = expr; Else; e3 = expr; { If(e, e2, e3) }
    | Match; e = expr; With; Pipe?; b = separated_nonempty_list(Pipe, match_branch); { MatchExpr(e, b) }
    | e = expr_appl; { e }

let expr_appl :=
    | e = expr_other; e2 = expr_appl; { Application(e, e2) }
    | e = expr_other; { e }

let expr_other :=
    | LParen; h = expr; Comma; t = separated_nonempty_list(Comma, expr); RParen; { Tuple (h :: t) }
    | LParen; ~ = expr; RParen; <>
    | ~ = Int; <IntExpr>
    | True; { BoolExpr(true) }
    | False; { BoolExpr(false) }
    | ~ = String; <StringExpr>
    | ~ = Id; <Identifier>
    | LParen; RParen; { Unit }

let match_branch :=
    | id = Id; vars = pattern_vars?; DoubleArrow; e = expr; { MatchBranch(id, Option.value ~default:[] vars, e) }

let pattern_vars :=
    | id = Id; { [id] }
    | LParen; ~ = separated_nonempty_list(Comma, Id); RParen; <>

let typ :=
    | ~ = typ_function; <>
    | ~ = typ_tuple; <>
    | ~ = typ_other; <>

let typ_function :=
    | t = typ_other; Arrow; t2 = typ; { FunType(t, t2) }

let typ_tuple :=
    | LParen; h = typ; Comma; t = separated_nonempty_list(Comma, typ); RParen; { TupleType (h :: t) }
    | t1 = typ_other; Times; t2 = typ; { TupleType [t1; t2] }

let typ_other :=
    | LParen; t = typ; RParen; { t }
    | id = Id; { IdType(id) }
    | TBool; { BoolType }
    | TInt; { IntType }
    | TString; { StringType }
    | TUnit; { UnitType }

let binop :=
    | Plus; { Add }
    | Minus; { Sub }
    | Times; { Mul }
    | Divide; { Div }
    | Mod; { Mod }
    | Lt; { Lt }
    | Eq; { Eq }
    | Concat; { Concat }
    | And; { And }
    | Or; { Or }

let unop :=
    | Negate; { Neg }
    | Not; { Not }