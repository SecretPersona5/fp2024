(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ENafikov_lib
open Parser
open Ast

(* TESTS  PARSER *)
let start_test parser show input =
  let res = start_parsing parser input in
  match res with
  | Ok res -> Format.printf "%s" (show res)
  | Error err -> Format.printf "%s" err
;;

(* Test const parser *)

let%expect_test _ =
  let test = "true" in
  start_test parse_bool show_const test;
  [%expect {| (Const_bool true) |}]
;;

let%expect_test _ =
  let test = "false" in
  start_test parse_bool show_const test;
  [%expect {| (Const_bool false) |}]
;;

let%expect_test _ =
  let test = "56894" in
  start_test parse_int show_const test;
  [%expect {| (Const_int 56894) |}]
;;

let%expect_test _ =
  let test = "\"I like Kakadu <3\"" in
  start_test parse_string show_const test;
  [%expect {| (Const_string "I like Kakadu <3") |}]
;;

let%expect_test _ =
  let test = "somethingname" in
  start_test parse_pattern_var show_pattern test;
  [%expect {| (Pattern_id "somethingname") |}]
;;

let%expect_test _ =
  let test = "somethinggg" in
  start_test parse_pattern show_pattern test;
  [%expect {| (Pattern_id "somethinggg") |}]
;;

let%expect_test _ =
  let test = "1" in
  start_test parse_pattern show_pattern test;
  [%expect {| (Pattern_const (Const_int 1)) |}]
;;

let%expect_test _ =
  let test = "\"SomeString\"" in
  start_test parse_pattern show_pattern test;
  [%expect {| (Pattern_const (Const_string "SomeString")) |}]
;;

let%expect_test _ =
  let test = "_" in
  start_test parse_pattern show_pattern test;
  [%expect {| Pattern_wild |}]
;;

let%expect_test _ =
  let test = "(1,2,3,4)" in
  start_test parse_pattern show_pattern test;
  [%expect
    {|
    (Pattern_tuple
       [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2));
         (Pattern_const (Const_int 3)); (Pattern_const (Const_int 4))]) |}]
;;

let%expect_test _ =
  let test = "[1;2;3;4]" in
  start_test parse_pattern show_pattern test;
  [%expect
    {|
    (Pattern_list ((Pattern_const (Const_int 1)),
       (Pattern_list ((Pattern_const (Const_int 2)),
          (Pattern_list ((Pattern_const (Const_int 3)),
             (Pattern_const (Const_int 4))))
          ))
       )) |}]
;;

let%expect_test _ =
  let test = "(1,2,3,[4;5])" in
  start_test parse_bind show_struct_prog test;
  [%expect
    {|
    (Expression
       (Expr_tuple
          [(Expr_const (Const_int 1)); (Expr_const (Const_int 2));
            (Expr_const (Const_int 3));
            (Expr_list ((Expr_const (Const_int 4)),
               (Expr_list ((Expr_const (Const_int 5)), (Expr_const Const_nil)))))
            ])) |}]
;;

let%expect_test _ =
  let test = "1 + 2" in
  start_test parse_bind show_struct_prog test;
  [%expect
    {|
    (Expression
       (Expr_bin_op (Add, (Expr_const (Const_int 1)), (Expr_const (Const_int 2))
          ))) |}]
;;

let%expect_test _ =
  let test = "let x = 42" in
  start_test parse_bind show_struct_prog test;
  [%expect {| (Let (false, "x", (Expr_const (Const_int 42)))) |}]
;;

let%expect_test _ =
  let test = "let rec fact n = if n = 1 then 1 else n * (fact (n - 1))" in
  start_test parse_bind show_struct_prog test;
  [%expect
    {|
    (Let (true, "fact",
       (Expr_fun ((Pattern_id "n"),
          (Expr_if (
             (Expr_bin_op (Eq, (Expr_var "n"), (Expr_const (Const_int 1)))),
             (Expr_const (Const_int 1)),
             (Expr_bin_op (Mul, (Expr_var "n"),
                (Expr_app ((Expr_var "fact"),
                   (Expr_bin_op (Sub, (Expr_var "n"), (Expr_const (Const_int 1))
                      ))
                   ))
                ))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  let test = "let x = 42 in x + 8" in
  start_test parse_expression show_expr test;
  [%expect
    {|
      (Expr_let_in (false, "x", (Expr_const (Const_int 42)),
         (Expr_bin_op (Add, (Expr_var "x"), (Expr_const (Const_int 8))))))|}]
;;
