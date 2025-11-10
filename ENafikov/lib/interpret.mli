(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

type runtime_env = (name, runtime_value, String.comparator_witness) Map.t

and runtime_value =
  | IntValue of int
  | BoolValue of bool
  | StringValue of string
  | UnitValue
  | ClosureValue of
      recursive_flag * pattern_node * pattern_node list * expr_node * runtime_env
  | ProductValue of runtime_value * runtime_value * runtime_value list
  | ListValue of runtime_value list
  | OptionalValue of runtime_value option
  | BuiltinFunction of (runtime_value -> (runtime_value, runtime_error) Result.t)

and runtime_error =
  | UnboundName of name
  | TypeMismatch
  | DivisionByZero
  | PatternMatchFailed
  | InvalidPattern

val print_runtime_error : Stdlib.Format.formatter -> runtime_error -> unit

module Eval : sig
  val evaluate_script : script -> (runtime_env, runtime_error) Result.t
end
