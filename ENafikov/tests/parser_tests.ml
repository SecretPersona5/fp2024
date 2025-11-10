(** Copyright 2024-2025, Ruslan Nafikov  *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ENafikov_lib
open Ast
open Parser
open Printf

let test_parsing input_string =
  match parse input_string with
  | Ok abstract_syntax_tree -> printf "%s\n" (show_script abstract_syntax_tree)
  | Error failure_message -> printf "Parse error: %s\n" failure_message
;;

let%expect_test "factorial_parsing" =
  test_parsing "let rec factorial n = if n < 2 then 1 else n * factorial(n - 1);;";
  [%expect
    {|
  [(BindingDeclaration (true,
      ((NamePattern "factorial"),
       (LambdaExpr ([(NamePattern "n")],
          (ConditionalExpr (
             (BinaryOpExpr (Less, (NameExpr "n"), (LiteralExpr (Integer 2)))),
             (LiteralExpr (Integer 1)),
             (Some (BinaryOpExpr (Multiply, (NameExpr "n"),
                      (ApplyExpr ((NameExpr "factorial"),
                         (BinaryOpExpr (Subtract, (NameExpr "n"),
                            (LiteralExpr (Integer 1))))
                         ))
                      )))
             ))
          ))),
      []))
    ]
|}]
;;

let%expect_test "fibonacci_parsing" =
  test_parsing "let rec fibo n = if n < 2 then 1 else fibo(n - 1) + fibo(n - 2) ;;";
  [%expect
    {|
  [(BindingDeclaration (true,
      ((NamePattern "fibo"),
       (LambdaExpr ([(NamePattern "n")],
          (ConditionalExpr (
             (BinaryOpExpr (Less, (NameExpr "n"), (LiteralExpr (Integer 2)))),
             (LiteralExpr (Integer 1)),
             (Some (BinaryOpExpr (Add,
                      (ApplyExpr ((NameExpr "fibo"),
                         (BinaryOpExpr (Subtract, (NameExpr "n"),
                            (LiteralExpr (Integer 1))))
                         )),
                      (ApplyExpr ((NameExpr "fibo"),
                         (BinaryOpExpr (Subtract, (NameExpr "n"),
                            (LiteralExpr (Integer 2))))
                         ))
                      )))
             ))
          ))),
      []))
    ]
|}]
;;

let%expect_test "lambda_parsing" =
  test_parsing "let add x = fun y -> x + y;;";
  [%expect
    {|
  [(BindingDeclaration (false,
      ((NamePattern "add"),
       (LambdaExpr ([(NamePattern "x")],
          (LambdaExpr ([(NamePattern "y")],
             (BinaryOpExpr (Add, (NameExpr "x"), (NameExpr "y")))))
          ))),
      []))
    ]
|}]
;;

let%expect_test "tuple_parsing" =
  test_parsing "let x = (1, 2, true) in x;;";
  [%expect
    {|
  [(ExprDeclaration
      (LetExpr (false,
         ((NamePattern "x"),
          (ProductExpr ((LiteralExpr (Integer 1)), (LiteralExpr (Integer 2)),
             [(LiteralExpr (Boolean true))]))),
         [], (NameExpr "x"))))
    ]
|}]
;;

let%expect_test "list_parsing" =
  test_parsing "let arr = [1;2;true]";
  [%expect
    {|
     [(BindingDeclaration (false,
         ((NamePattern "arr"),
          (ListExpr
             [(LiteralExpr (Integer 1)); (LiteralExpr (Integer 2));
               (LiteralExpr (Boolean true))])),
         []))
       ]
|}]
;;

let%expect_test "single_element_parsing" =
  test_parsing "let x = (666)";
  [%expect
    {|
     [(BindingDeclaration (false,
         ((NamePattern "x"), (LiteralExpr (Integer 666))), []))
       ]
|}]
;;

let%expect_test "multi_parameter_function" =
  test_parsing "let sum x y = x + y";
  [%expect
    {|
[(BindingDeclaration (false,
    ((NamePattern "sum"),
     (LambdaExpr ([(NamePattern "x"); (NamePattern "y")],
        (BinaryOpExpr (Add, (NameExpr "x"), (NameExpr "y")))))),
    []))
  ]
|}]
;;

let%expect_test "type_annotation_parsing" =
  test_parsing "let sum (x:int) (y:int) = x + y";
  [%expect
    {|
[(BindingDeclaration (false,
    ((NamePattern "sum"),
     (LambdaExpr (
        [(AnnotatedPattern ((NamePattern "x"), (BaseType "int")));
          (AnnotatedPattern ((NamePattern "y"), (BaseType "int")))],
        (BinaryOpExpr (Add, (NameExpr "x"), (NameExpr "y")))))),
    []))
  ]
|}]
;;

let%expect_test "list_type_annotation" =
  test_parsing "let (a : int list) = [] ";
  [%expect
    {|
[(BindingDeclaration (false,
    ((AnnotatedPattern ((NamePattern "a"), (ListType (BaseType "int")))),
     (ListExpr [])),
    []))
  ]
|}]
;;

let%expect_test "complex_arithmetic_parsing" =
  test_parsing "-1 -2 - (-1) -(3)";
  [%expect
    {|
[(ExprDeclaration
    (BinaryOpExpr (Subtract,
       (BinaryOpExpr (Subtract,
          (BinaryOpExpr (Subtract,
             (UnaryOpExpr (Negate, (LiteralExpr (Integer 1)))),
             (LiteralExpr (Integer 2)))),
          (UnaryOpExpr (Negate, (LiteralExpr (Integer 1)))))),
       (LiteralExpr (Integer 3)))))
  ]
 |}]
;;

let%expect_test "unit_pattern_parsing" =
  test_parsing "let () = print_int 5";
  [%expect
    {|
[(BindingDeclaration (false,
    (UnitPattern,
     (ApplyExpr ((NameExpr "print_int"), (LiteralExpr (Integer 5))))),
    []))
  ]
 |}]
;;