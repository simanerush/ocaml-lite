open OUnit2
open Ocaml_lite.Ast
open Ocaml_lite.Parser
open Ocaml_lite.Dump

let assert_equal_expr s e = 
  assert_equal
    (parse ("let x = " ^ s ^ ";;"))
    ([ValueBinding ("x", [], None, e)])

let test_parser_value_binding _ =
  assert_equal
    (parse "let x = 5;;")
    ([ValueBinding ("x", [], None, IntExpr 5)])

let test_parser_val_bind_arith _ = 
  assert_equal
    (parse "let add x y = x + y;;")
    ([ValueBinding ("add", [SimpleParam "x"; SimpleParam "y"], None, BinaryOp (Add, Identifier "x", Identifier "y"))])

let test_parser_rec_val_bind_cond _ = 
  assert_equal
    (parse "let rec fact n = if n = 0 then 1 else n * fact (n - 1);;")
    ([RecursiveBinding ("fact", [SimpleParam "n"], None, If (BinaryOp (Eq, Identifier "n", IntExpr 0), IntExpr 1, BinaryOp (Mul, Identifier "n", Application (Identifier "fact", BinaryOp (Sub, Identifier "n", IntExpr 1)))))])

let test_parser_type_def _ = 
  assert_equal
    (parse "type color = | Red | Green | Blue;;")
    ([TypeBinding ("color", [Constructor ("Red", None); Constructor ("Green", None); Constructor ("Blue", None)])])

let test_parser_tuple _ = 
  assert_equal
    (parse "let pair = (3, true);;")
    ([ValueBinding ("pair", [], None, Tuple [IntExpr 3; BoolExpr true])])

let test_parser_fun_comp _ =
  assert_equal
    (parse "let f = fun x y => x < y;;")
    ([ValueBinding ("f", [], None, FunExpr ([SimpleParam "x"; SimpleParam "y"], None, BinaryOp (Lt, Identifier "x", Identifier "y")))])

let test_parser_typed_fun _ =
  assert_equal
    (parse "let f = fun (x : int) (y : int) => x < y;;")
    ([ValueBinding ("f", [], None, FunExpr ([TypedParam ("x", IntType) ; TypedParam ("x", IntType)], None, BinaryOp (Lt, Identifier "x", Identifier "y")))])

let test_parser_unary_o _ =
  assert_equal
    (parse "let x = ~5;;")
    ([ValueBinding ("x", [], None, UnaryOp (Neg, IntExpr 5))])

let test_parser_string _ = 
  assert_equal
    (parse "let x = \"hello\";;")
    ([ValueBinding ("x", [], None, StringExpr "hello")])

let test_parser_pattern_match _ =
  assert_equal_expr
  "match x with | Some n => n | None => 0"
  (MatchExpr (
    Identifier "x", 
    [
      MatchBranch ("Some", ["n"], Identifier "n");
      MatchBranch ("None", [], IntExpr 0)
    ]
  ))

let parse_tests = 
  "test suite for parser"
  >::: [
    "value binding" >:: test_parser_value_binding;
    "value binding with arithmetic" >:: test_parser_val_bind_arith;
    "recursive value binding with condition" >:: test_parser_rec_val_bind_cond;
    "type definition" >:: test_parser_type_def;
    "tuple creation" >:: test_parser_tuple;
    "function with comparison" >:: test_parser_fun_comp;
    "unary operator" >:: test_parser_unary_o;
    "string" >:: test_parser_string;
    "pattern matching" >:: test_parser_pattern_match;
  ]
