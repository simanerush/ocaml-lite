open OUnit2
open Ocaml_lite.Typechecker
open Ocaml_lite.Parser

exception ParseError of string

let assert_well_typed (expr_str: string) (expected: bool) _ =
  let ast = parse_string expr_str in
  match type_check ast with
  | Ok _ when expected -> ()
  | Error _ when not expected -> ()
  | Ok _ ->
      assert_failure 
        (Printf.sprintf "Type check succeeded unexpectedly for expression: '%s'" expr_str)
  | Error err ->
      assert_failure 
        (Printf.sprintf "Type check failed unexpectedly for expression: '%s'. Error: %s" expr_str err)
  
let test_unary_op test_ctxt =
  assert_well_typed "let a = ~5;;" true test_ctxt

let test_fun_with_multiple_args test_ctxt =
  assert_well_typed "let a = fun (x : int) (y : int) => x + y;; let b = a 5 6;;" true test_ctxt

let test_nested_let_bindings test_ctxt =
  assert_well_typed "let a = let b = 5 in b + 3;;" true test_ctxt

let test_type_mismatch test_ctxt =
  assert_well_typed "let a = 5 + true;;" false test_ctxt

let test_if_condition_type test_ctxt =
  assert_well_typed "let a = if 5 then 4 else 3;;" false test_ctxt

let test_if_branch_type_mismatch test_ctxt =
  assert_well_typed "let a = if true then 4 else \"c\";;" false test_ctxt

let test_tuple_type test_ctxt =
  assert_well_typed "let a : int * bool = (5, true);;" true test_ctxt

let test_tuple_type_mismatch test_ctxt =
  assert_well_typed "let a : int * int = (5, true);;" false test_ctxt

let test_recursive_function test_ctxt =
  assert_well_typed "let rec fact n = if n = 0 then 1 else n * fact (n - 1);; let a = fact 5;;" true test_ctxt

let test_recursive_function_type_error test_ctxt =
  assert_well_typed "let rec fact n = if n = false then 1 else n * fact (n - 1);; let a = fact 5;;" false test_ctxt

let test_pattern_matching_on_custom_type test_ctxt =
  assert_well_typed "type color = | Red | Green | Blue;; let string_of_color = fun c => match c with | Red => \"red\" | Green => \"green\" | Blue => \"blue\";; let color_str : string = string_of_color Red;;" true test_ctxt

let typechecker_tests = 
  "test quite for typechecker" >::: [
    "unary operation" >:: test_unary_op;
    "function with multiple arguments" >:: test_fun_with_multiple_args;
    "nested let bindings" >:: test_nested_let_bindings;
    "type mismatch" >:: test_type_mismatch;
    "if condition" >:: test_if_condition_type;
    "if branch with type mismatch" >:: test_if_branch_type_mismatch;
    "tuple type" >:: test_tuple_type;
    "tuple type mismatch" >:: test_tuple_type_mismatch;
    "recursive function" >:: test_recursive_function;
    "recursive function with type error" >:: test_recursive_function_type_error;
    "pattern matching on custom type" >:: test_pattern_matching_on_custom_type;
  ]