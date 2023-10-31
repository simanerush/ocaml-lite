open OUnit2

let assert_well_typed (expr: string) (expected: bool) =
  assert_equal (failwith "Typechecker is not implemented") expected

let test_unary_op _ =
  assert_well_typed "let a = ~5;;" true

let test_fun_with_multiple_args _ =
  assert_well_typed "let a = fun (x : int) (y : int) => x + y;; let b = a 5 6;;" true

let test_nested_let_bindings _ =
  assert_well_typed "let a = let b = 5 in b + 3;;" true

let test_type_mismatch _ =
  assert_well_typed "let a = 5 + true;;" false

let test_if_condition_type _ =
  assert_well_typed "let a = if 5 then 4 else 3;;" false

let test_if_branch_type_mismatch _ =
  assert_well_typed "let a = if true then 4 else \"c\";;" false

let test_tuple_type _ =
  assert_well_typed "let a : int * bool = (5, true);;" true

let test_tuple_type_mismatch _ =
  assert_well_typed "let a : int * int = (5, true);;" false

let test_recursive_function _ =
  assert_well_typed "let rec fact n = if n = 0 then 1 else n * fact (n - 1);; let a = fact 5;;" true

let test_recursive_function_type_error _ =
  assert_well_typed "let rec fact n = if n then 1 else n * fact (n - 1);; let a = fact 5;;" false

let test_pattern_matching_on_custom_type _ =
  assert_well_typed "type color = | Red | Green | Blue;; let string_of_color = fun c => match c with | Red => 'red' | Green => 'green' | Blue => 'blue';; let color_str : string = string_of_color Red;;" true

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