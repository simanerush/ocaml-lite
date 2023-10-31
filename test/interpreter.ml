open OUnit2

type ol_value =
  | OLInt of int
  | OLBool of bool
  | OLString of string
  | OLUnit
  | OLTuple of ol_value * ol_value
  | OLFunction of (ol_value list) -> ol_value

let assert_interprets_to (expr : string) (expected : ol_value) =
  assert_equal (failwith "Interpreter is not implemented") expected

let test_arithmetic _ =
  assert_interprets_to "let a = 3 + 4;;" (OLInt 7)

let test_conditional_true _ =
  assert_interprets_to "if true then 5 else 10;;" (OLInt 5)

let test_conditional_false _ =
  assert_interprets_to "if false then 5 else 10;;" (OLInt 10)

let test_string_concat _ =
  assert_interprets_to "let a = \"hello\" ^ \" world\";;" (OLString "hello world")

let test_function_simple _ =
  assert_interprets_to "let a = fun x => x + 1;; a 5;;" (OLInt 6)

let test_function_complex _ =
  assert_interprets_to "let rec a b = if b = 0 then 1 else b * a (b - 1);; a 5;;" (OLInt 120)

let test_tuple _ =
  assert_interprets_to "let a = (5, true);;" (OLTuple [OLInt 5; OLBool true])

let test_let_binding _ =
  assert_interprets_to "let a = 5;; let b = a + 1;;" (OLInt 6)

let test_unary_operation _ =
  assert_interprets_to "let a = ~5;;" (OLInt (-5))

let test_match _ =
  assert_interprets_to "let a = match 5 with | 5 => true | _ => false;;" (OLBool true)

let test_recursion _ =
  assert_interprets_to "let rec fact x = if x = 1 then 1 else x * fact (x - 1);; fact 5;;" (OLInt 120)

let interpreter_tests =
  "test suite for interpreter"
  >::: [
    "arithmetic" >:: test_arithmetic;
    "conditional true" >:: test_conditional_true;
    "conditional false" >:: test_conditional_false;
    "string concatenation" >:: test_string_concat;
    "simple function" >:: test_function_simple;
    "complex function" >:: test_function_complex;
    "tuple" >:: test_tuple;
    "let binding" >:: test_let_binding;
    "unary operation" >:: test_unary_operation;
    "match" >:: test_match;
    "recursion" >:: test_recursion;
  ]