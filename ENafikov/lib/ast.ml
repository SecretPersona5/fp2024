(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

type name = string [@@deriving show { with_path = false }]
type recursive_flag = bool [@@deriving show { with_path = false }]

type binary_op =
  | Add
  | Subtract
  | Multiply
  | Divide
  | LogicalAnd
  | LogicalOr
  | GreaterOrEqual
  | LessOrEqual
  | Greater
  | Less
  | Equal
  | NotEqual
[@@deriving show { with_path = false }]

type unary_op =
  | Negate
  | LogicalNot
[@@deriving show { with_path = false }]

type literal =
  | Integer of int
  | Boolean of bool
  | Text of string
[@@deriving show { with_path = false }]

type type_var = int [@@deriving show { with_path = false }]

type type_expr =
  | TypeVar of type_var
  | BaseType of string
  | FuncType of type_expr * type_expr
  | ListType of type_expr
  | ProductType of type_expr list
  | OptionalType of type_expr
[@@deriving show { with_path = false }]

type pattern_node =
  | NamePattern of name
  | LiteralPattern of literal
  | ProductPattern of pattern_node * pattern_node * pattern_node list
  | WildcardPattern
  | AnnotatedPattern of pattern_node * type_expr
  | UnitPattern
  | ListPattern of pattern_node list
  | OptionalPattern of pattern_node option
[@@deriving show { with_path = false }]

type expr_node =
  | NameExpr of name
  | LiteralExpr of literal
  | ConditionalExpr of expr_node * expr_node * expr_node option
  | BinaryOpExpr of binary_op * expr_node * expr_node
  | UnaryOpExpr of unary_op * expr_node
  | ProductExpr of expr_node * expr_node * expr_node list
  | ListExpr of expr_node list
  | LambdaExpr of pattern_node list * expr_node
  | TypeAnnotationExpr of expr_node * type_expr
  | LetExpr of recursive_flag * binding * binding list * expr_node
  | ApplyExpr of expr_node * expr_node
  | OptionalExpr of expr_node option
[@@deriving show { with_path = false }]

and binding = pattern_node * expr_node [@@deriving show { with_path = false }]

type declaration =
  | ExprDeclaration of expr_node
  | BindingDeclaration of recursive_flag * binding * binding list
[@@deriving show { with_path = false }]

type script = declaration list [@@deriving show { with_path = false }]

let rec print_type fmt = function
  | BaseType x -> fprintf fmt "%s" x
  | TypeVar x -> fprintf fmt "'%d" x
  | FuncType (l, r) ->
    (match l, r with
     | FuncType _, _ -> fprintf fmt "(%a) -> %a" print_type l print_type r
     | _, _ -> fprintf fmt "%a -> %a" print_type l print_type r)
  | ProductType elements ->
    fprintf
      fmt
      "(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " * ") print_type)
      elements
  | ListType ty ->
    (match ty with
     | FuncType _ | ProductType _ -> fprintf fmt "(%a) list" print_type ty
     | _ -> fprintf fmt "%a list" print_type ty)
  | OptionalType ty ->
    (match ty with
     | FuncType _ | ProductType _ -> fprintf fmt "(%a) option" print_type ty
     | _ -> fprintf fmt "%a option" print_type ty)
;;
