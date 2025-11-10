(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ENafikov_lib
open Inferencer
open Ast

let pretty_print_inference_result source =
  match Parser.parse source with
  | Ok parsed_ast ->
    (match run_inference parsed_ast with
     | Ok type_environment ->
       let filtered_environment =
         Base.Map.filter_keys type_environment ~f:(fun key ->
           not (List.mem key [ "print_int"; "print_endline"; "print_bool" ]))
       in
       Base.Map.iteri filtered_environment ~f:(fun ~key ~data:(Forall (_, type_expr)) ->
         Format.printf "val %s: %a\n" key print_type type_expr)
     | Error inference_error ->
       Format.printf "Type inference error. %a\n" print_error inference_error)
  | Error parse_error -> Format.printf "Parsing error. %s\n" parse_error
;;

let pretty_print_simple_expression_type source =
  match Parser.parse source with
  | Ok parsed_ast ->
    (match parsed_ast with
     | [ ExprDeclaration expr_node ] ->
       (match infer_simple_expr expr_node with
        | Ok type_expr -> Format.printf "%a\n" print_type type_expr
        | Error inference_error ->
          Format.printf "Type inference error. %a\n" print_error inference_error)
     | _ ->
       Format.printf
         "Expected a single expression, but got a program with multiple structures.\n")
  | Error parse_error -> Format.printf "Parsing error. %s\n" parse_error
;;

let%expect_test "test_arithmetic_operations" =
  pretty_print_simple_expression_type "10/2 + 56*2 - 10 / 10 / 20 + 666 - 777 + 1";
  [%expect {|int|}]
;;

let%expect_test "test_boolean_literal" =
  pretty_print_simple_expression_type "false";
  [%expect {|bool|}]
;;

let%expect_test "test_string_literal" =
  pretty_print_simple_expression_type "\"I like OCaml\" ";
  [%expect {|string|}]
;;

let%expect_test "test_option_type" =
  pretty_print_simple_expression_type "Some 10";
  [%expect {|int option|}]
;;

let%expect_test "test_function_with_arithmetic" =
  pretty_print_simple_expression_type "fun x -> x * 69 + 100 - 201 / 777";
  [%expect {|int -> int|}]
;;

let%expect_test "test_recursive_function" =
  pretty_print_inference_result "let rec func arg = func arg";
  [%expect {|val func: '1 -> '2|}]
;;

let%expect_test "test_function_application" =
  pretty_print_inference_result "let func a1 a2 a3 = a1 a2 a3";
  [%expect {|val func: ('1 -> '2 -> '4) -> '1 -> '2 -> '4|}]
;;

let%expect_test "test_tuple_function" =
  pretty_print_simple_expression_type "fun x y z -> (x + 10, y / 2 , z)";
  [%expect {|int -> int -> '2 -> (int * int * '2)|}]
;;

let%expect_test "test_list_creation" =
  pretty_print_inference_result "let arr = [1;2;3]";
  [%expect {|val arr: int list|}]
;;

let%expect_test "test_comparison_function" =
  pretty_print_inference_result "let is_above_10 x = if x > 10 then true else false ";
  [%expect {|val is_above_10: int -> bool|}]
;;

let%expect_test "test_simple_comparison" =
  pretty_print_inference_result "let is_above_10 x = x > 10";
  [%expect {|val is_above_10: int -> bool|}]
;;

let%expect_test "test_factorial_function" =
  pretty_print_inference_result "let rec fac n = if n < 2 then 1 else n * fac (n - 1)";
  [%expect {|val fac: int -> int|}]
;;

let%expect_test "test_nested_list_function" =
  pretty_print_inference_result "let f x = [ [x; x]; [x] ]";
  [%expect {|val f: '0 -> '0 list list|}]
;;

let%expect_test "test_option_function" =
  pretty_print_inference_result "let f x = Some x";
  [%expect {|val f: '0 -> '0 option|}]
;;

let%expect_test "test_fibonacci_function" =
  pretty_print_inference_result
    "let rec fibo n = if n < 2 then 1 else fibo(n - 1) + fibo(n - 2)";
  [%expect {|val fibo: int -> int|}]
;;

let%expect_test "test_unbound_variable" =
  pretty_print_inference_result "let f = x";
  [%expect {|Type inference error. Unknown name 'x'.|}]
;;

let%expect_test "test_type_annotation" =
  pretty_print_inference_result "let sum = fun (x : int) (y : int) -> x + y";
  [%expect {|val sum: int -> int -> int|}]
;;

let%expect_test "test_annotated_factorial" =
  pretty_print_inference_result
    "let rec fac = fun (n : int) (acc : int) -> if n < 2 then acc else fac (n-1) (acc * \
     n);;";
  [%expect {|val fac: int -> int -> int|}]
;;

let%expect_test "test_multiple_functions" =
  pretty_print_inference_result
    "let div = fun x y -> x / y \n\
    \     let sum = fun x y -> x + y\n\
    \     let res = fun x y z -> div x (sum y z)";
  [%expect
    {|
    val div: int -> int -> int
    val res: int -> int -> int -> int
    val sum: int -> int -> int|}]
;;

let%expect_test "test_square_function" =
  pretty_print_inference_result
    "let square = fun x -> x * x\n\
    \                                  let result = square 10";
  [%expect {|
    val result: int
    val square: int -> int|}]
;;

let%expect_test "test_type_mismatch_annotation" =
  pretty_print_inference_result "let sum (x : int) (y : string) = x + y";
  [%expect {|Type inference error. TypeMismatch: string and int.|}]
;;

let%expect_test "test_unification_error" =
  pretty_print_inference_result "fun x -> x + true";
  [%expect {|Type inference error. TypeMismatch: bool and int.|}]
;;

let%expect_test "test_option_type_error" =
  pretty_print_inference_result
    "let f x = Some (x + 1) in let g y = Some (y && true) in f = g";
  [%expect {|Type inference error. TypeMismatch: bool and int.|}]
;;
