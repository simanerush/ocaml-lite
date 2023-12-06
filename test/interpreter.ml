open OUnit2
open Ocaml_lite.Parser
open Ocaml_lite.Ast
open Ocaml_lite.Interpreter

let rec find_value_by_id id ctx =
  match ctx with
  | (ctx_id, value) :: rest -> if ctx_id = id then Some value else find_value_by_id id rest
  | [] -> None

let assert_interprets_to (expr: string) (id: string) (expected: ol_value) =
  let ast = parse expr in
  let ctx = interpret ast in
  match find_value_by_id id ctx with
  | Some value -> assert_equal expected value
  | None -> assert_failure ("Identifier \"" ^ id ^ "\" not found in context")

let test_arithmetic _ =
  assert_interprets_to "let a = 3 + 4;;" "a" (OLInt 7)

let test_conditional_true _ =
  assert_interprets_to "let b = if true then 5 else 10;;" "b" (OLInt 5)

let test_conditional_false _ =
  assert_interprets_to "let c = if false then 5 else 10;;" "c" (OLInt 10)

let test_string_concat _ =
  assert_interprets_to "let a = \"hello\" ^ \" world\";;" "a" (OLString "hello world")

let test_function_simple _ =
  assert_interprets_to "let a = fun x => x + 1;; let result = a 5;;" "result" (OLInt 6)

let test_function_complex _ =
  assert_interprets_to "let rec a b = if b = 0 then 1 else b * a (b - 1);; let result = a 5;;" "result" (OLInt 120)

let test_tuple _ =
  assert_interprets_to "let a = (5, true);;" "a" (OLTuple [OLInt 5; OLBool true])

let test_let_binding _ =
  assert_interprets_to "let a = 5;; let b = a + 1;;" "b" (OLInt 6)

let test_unary_operation _ =
  assert_interprets_to "let a = ~5;;" "a" (OLInt (-5))

let test_match _ =
  let program = "type color = | Red | Green | Blue;; \
                  let string_of_color = fun c => match c with \
                  | Red => \"red\" \
                  | Green => \"green\" \
                  | Blue => \"blue\";; \
                  let color_str : string = string_of_color Red;;" in
  assert_interprets_to program "color_str" (OLString "red") 

let test_recursion _ =
  assert_interprets_to "let rec fact x = if x = 1 then 1 else x * fact (x - 1);; let result = fact 5;;" "result" (OLInt 120)

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