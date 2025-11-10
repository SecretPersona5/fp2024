(** Copyright 2024-2025, Ruslan Nafikov  *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ENafikov_lib
open Interpret

let run_evaluation_test source_code =
  let open Stdlib.Format in
  match Parser.parse source_code with
  | Ok parsed_ast ->
    (match Eval.evaluate_script parsed_ast with
     | Ok _ -> ()
     | Error runtime_error -> printf "Runtime error: %a\n" print_runtime_error runtime_error)
  | Error parse_error -> printf "Parsing error: %s\n" parse_error
;;

let%expect_test "test_unit_output" =
  run_evaluation_test "let () = print_int(10 / 10 + 2 * 50 + 89 - 89)";
  [%expect {|101|}]
;;

let%expect_test "test_boolean_operations" =
  run_evaluation_test
    "let () = print_bool(true) in\n\
    \                  let () = print_bool(false) in\n\
    \                  let () = print_bool(not true) in \n\
    \                  let () = print_bool(not false) in\n\
    \                  let () = print_bool(true && false) in\n\
    \                  let () = print_bool(true || false ) in 9";
  [%expect {|
    true
    false
    false
    true
    false
    true|}]
;;

let%expect_test "test_comparison_operators" =
  run_evaluation_test
    "let a = 1\n\
    \                  let b = 2\n\
    \                  let () = print_bool(a = a)\n\
    \                  let () = print_bool(b > a)\n\
    \                  let () = print_bool(a < b)\n\
    \                  let () = print_bool(a <> b)\n\
    \                  let () = print_bool(a <> a)\n\
    \                  let () = print_bool(a <= a)\n\
    \                  let () = print_bool(a >= a)\n\
    \                  let () = print_bool(a <= b)\n\
    \                  let () = print_bool(a >= b)";
  [%expect
    {|
    true
    true
    true
    true
    false
    true
    true
    true
    false|}]
;;

let%expect_test "test_closure_creation" =
  run_evaluation_test
    "let create_adder x =\n\
    \                  let adder y = x + y in\n\
    \                  adder\n\
    \                  let sum_two_arg = print_int(create_adder 10 20)";
  [%expect {|30|}]
;;

let%expect_test "test_lambda_expression" =
  run_evaluation_test
    "let create_adder = fun x -> fun y -> x + y\n\
    \     let () = print_int(create_adder 7 8)";
  [%expect {|15|}]
;;

let%expect_test "test_string_output" =
  run_evaluation_test "let () = print_endline \"I like OCaml\"";
  [%expect {|I like OCaml|}]
;;

let%expect_test "test_silent_execution" =
  run_evaluation_test
    "let create_adder x =\n\
    \                  let adder y = x + y in\n\
    \                  adder\n\
    \                  let fac n = if n < 2 then 1 else n * fac(n-1) \n\
    \                      let x = 1\n\
    \                      let y = true";
  [%expect {||}]
;;

let%expect_test "test_factorial_calculation" =
  run_evaluation_test
    "let rec fac n = if n < 2 then 1 else n * fac(n-1) \n\
    \     let result = print_int(fac 5)";
  [%expect {|120|}]
;;

let%expect_test "test_factorial_cps" =
  run_evaluation_test
    "let rec fac_cps n k =\n\
    \                  if n=1 then k 1 else\n\
    \                  fac_cps (n-1) (fun p -> k (p*n))\n\
    \                  let result = print_int(fac_cps 5 (fun x -> x))";
  [%expect {|120|}]
;;

let%expect_test "test_fibonacci_calculation" =
  run_evaluation_test
    "let rec fibo n = if n < 2 then 1 else fibo(n-1) + fibo(n-2)\n\
    \                  let result = print_int(fibo 5)";
  [%expect {|8|}]
;;

let%expect_test "test_fixed_point_combinator" =
  run_evaluation_test
    "let rec fix f x = f (fix f) x\n\
    \                  let fac self n = if n<=1 then 1 else n * self (n-1)\n\
    \                  let f = print_int (fix fac 5)";
  [%expect {|120|}]
;;

let%expect_test "test_nested_closure" =
  run_evaluation_test
    "\n\
    \    let rec outer x =\n\
    \      let rec inner y = x + y in\n\
    \      inner\n\
    \    let inner = outer 10\n\
    \    let () = print_int (inner 5)";
  [%expect {|15|}]
;;

let%expect_test "test_annotated_addition" =
  run_evaluation_test "let sum (x : int) (y : int) = x + y let res = print_int(sum 10 20)";
  [%expect {|30|}]
;;

let%expect_test "test_annotated_factorial" =
  run_evaluation_test
    "let rec fac (n : int) (acc : int) = if n < 2 then acc else fac (n-1) (acc * n)\n\
    \                  let res = print_int (fac 5 1)";
  [%expect {|120|}]
;;

let%expect_test "test_tuple_destructuring" =
  run_evaluation_test
    "let (a,b) = (1 + 1 * 10,2 - 1 * 5)\n\
    \     let () = print_int a \n\
    \     let () = print_int b";
  [%expect {|
    11
    -3|}]
;;

let%expect_test "test_nested_tuple" =
  run_evaluation_test
    "\n\
    \    let (a, b) = (1 + 2, 3 * 4)\n\
    \        let (c, d) = (a + b, b - a)\n\
    \        let () = print_int c\n\
    \        let () = print_int d";
  [%expect {|
    15
    9|}]
;;

let%expect_test "test_list_pattern_matching" =
  run_evaluation_test
    "let lst = [1;2;3]\n\
    \    let [a; b; c] = lst in \n\
    \    let () = print_int(a) in\n\
    \    let () = print_int(b) in \n\
    \    let () = print_int(c) in 0";
  [%expect {|
    1
    2
    3|}]
;;

let%expect_test "test_lexical_scope" =
  run_evaluation_test
    "let x = \n\
    \      let y = \n\
    \        let z = \n\
    \          let w = 1\n\
    \          in w\n\
    \        in z\n\
    \      in y\n\
    \    \n\
    \    let () = print_int x";
  [%expect {|1|}]
;;

let%expect_test "test_mutual_recursion" =
  run_evaluation_test
    {|
  let rec factorial n = if n <= 1 then 1 else n * helper (n - 1)
  and helper x = factorial x in
  let () = print_int (factorial 5) in 0
  |};
  [%expect {|
    120
  |}]
;;

let%expect_test "test_division_by_zero" =
  run_evaluation_test "let div = fun x y -> x / y\n                  let res = div 10 0";
  [%expect {|Runtime error: DivisionByZero|}]
;;

let%expect_test "test_pattern_matching_error" =
  run_evaluation_test "let (a, b) = (1,2,3)";
  [%expect {|Runtime error: PatternMatchFailed|}]
;;

let%expect_test "test_unbound_variable_error" =
  run_evaluation_test "let x = x + 1";
  [%expect {|Runtime error: UnboundName: "x"|}]
;;

let%expect_test "test_type_error_in_addition" =
  run_evaluation_test "let x = 10 + true";
  [%expect {|Runtime error: TypeMismatch|}]
;;

let%expect_test "test_invalid_recursive_pattern" =
  run_evaluation_test "let rec (a, b) = (1,2)";
  [%expect {|Runtime error: InvalidPattern|}]
;;