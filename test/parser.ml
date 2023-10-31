open OUnit2
open Ocaml_lite.Ast

let p_assert_equal code expr = failwith "Parser is not implemented"

let test_parser_value_binding _ =
  p_assert_equal
    "let x = 5;;"
    [ValueBinding ("x", [], None, Int 5)]

let test_parser_val_bind_arith _ = 
  p_assert_equal
    "let add x y = x + y;;"
    [ValueBinding ("add", [SimpleParam "x"; SimpleParam "y"], None, BinaryOp (Add, Identifier "x", Identifier "y"))]

let test_parser_rec_val_bind_cond _ = 
  p_assert_equal
  "let rec fact n = if n = 0 then 1 else n * fact (n - 1);;"
  [RecursiveBinding ("fact", [SimpleParam "n"], None, If (BinaryOp (Eq, Identifier "n", Int 0), Int 1, BinaryOp (Mul, Identifier "n", Application (Identifier "fact", BinaryOp (Sub, Identifier "n", Int 1)))))]

let test_parser_type_def _ = 
  p_assert_equal
    "type color = | Red | Green | Blue;;"
    [TypeBinding ("color", [Constructor ("Red", None); Constructor ("Green", None); Constructor ("Blue", None)])]

let test_parser_tuple _ = 
  p_assert_equal
    "let pair = (3, true);;"
    [ValueBinding ("pair", [], None, Tuple [Int 3; Bool true])]

let test_parser_fun_comp _ =
  p_assert_equal
    "let f = fun x y => x < y;;"
    [ValueBinding ("f", [], None, Fun ([SimpleParam "x"; SimpleParam "y"], None, BinaryOp (Lt, Identifier "x", Identifier "y")))]

let test_parser_unary_o _ =
  p_assert_equal
    "let x = ~5;;"
    [ValueBinding ("x", [], None, UnaryOp (Neg, Int 5))]

let test_parser_string _ = 
  p_assert_equal
    "let x = \"hello\";;"
    [ValueBinding ("x", [], None, String "hello")]

let test_parser_pattern_match _ =
  p_assert_equal
  "match x with | Some n => n | None => 0;;"
  [ValueBinding ("x", [], None, Match (Identifier "x", [MatchBranch ("Some", [SimpleVar "n"], Identifier "n"); MatchBranch ("None", [], Int 0)]))]

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
