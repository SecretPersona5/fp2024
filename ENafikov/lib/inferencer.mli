(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

module IntSet : sig
  type t = Stdlib.Set.Make(Int).t
end

type type_error =
  | CircularCheck of int * type_expr
  | UnknownName of string
  | TypeMismatch of type_expr * type_expr
  | MultipleBounds of string
  | PatternError of string
  | ExpressionError of string
  | UnexpectedArrow of type_expr

val print_error : Stdlib.Format.formatter -> type_error -> unit

module TypeScheme : sig
  type scheme = Forall of IntSet.t * type_expr
end

module Context : sig
  type context = (name, TypeScheme.scheme, String.comparator_witness) Map.t
end

val infer_simple_expr : expr_node -> (type_expr, type_error) Result.t
val run_inference : Ast.script -> (Context.context, type_error) Result.t
