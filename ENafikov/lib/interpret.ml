(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Stdlib.Format

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

let print_runtime_error fmt = function
  | UnboundName ident -> fprintf fmt "UnboundName: %S" ident
  | TypeMismatch -> fprintf fmt "TypeMismatch"
  | DivisionByZero -> fprintf fmt "DivisionByZero"
  | PatternMatchFailed -> fprintf fmt "PatternMatchFailed"
  | InvalidPattern -> fprintf fmt "InvalidPattern"
;;

module type EvalMonad = sig
  type ('a, 'e) monad

  val return : 'a -> ('a, 'e) monad
  val fail : 'e -> ('a, 'e) monad
  val ( let* ) : ('a, 'e) monad -> ('a -> ('b, 'e) monad) -> ('b, 'e) monad
end

module EnvOps (M : EvalMonad) = struct
  open M

  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let lookup map key =
    match Map.find map key with
    | Some value -> return value
    | None -> fail (UnboundName key)
  ;;
end

module Evaluator (M : EvalMonad) : sig
  val evaluate_script : script -> (runtime_env, runtime_error) M.monad
end = struct
  open M
  open EnvOps (M)

  let initial_env =
    let open Base.Map in
    empty (module String)
    |> set
         ~key:"print_int"
         ~data:
           (BuiltinFunction
              (function
                | IntValue i ->
                  Stdlib.print_int i;
                  Stdlib.print_newline ();
                  Result.return UnitValue
                | _ -> Result.fail TypeMismatch))
    |> set
         ~key:"print_endline"
         ~data:
           (BuiltinFunction
              (function
                | StringValue s ->
                  Stdlib.print_endline s;
                  Result.return UnitValue
                | _ -> Result.fail TypeMismatch))
    |> set
         ~key:"print_bool"
         ~data:
           (BuiltinFunction
              (function
                | BoolValue b ->
                  Stdlib.print_string (Bool.to_string b);
                  Stdlib.print_newline ();
                  Result.return UnitValue
                | _ -> Result.fail TypeMismatch))
  ;;

  let rec match_pattern env = function
    | WildcardPattern, _ -> Some env
    | UnitPattern, UnitValue -> Some env
    | LiteralPattern (Integer i1), IntValue i2 when i1 = i2 -> Some env
    | LiteralPattern (Boolean b1), BoolValue b2 when Bool.equal b1 b2 -> Some env
    | LiteralPattern (Text s1), StringValue s2 when String.equal s1 s2 -> Some env
    | NamePattern x, v -> Some (extend env x v)
    | AnnotatedPattern (pat, _), v -> match_pattern env (pat, v)
    | ProductPattern (p1, p2, pl), ProductValue (v1, v2, vl) ->
      (match match_pattern env (p1, v1) with
       | None -> None
       | Some env1 ->
         (match match_pattern env1 (p2, v2) with
          | None -> None
          | Some env2 ->
            (match
               List.fold2 pl vl ~init:(Some env2) ~f:(fun acc_env p v ->
                 match acc_env with
                 | Some env' -> match_pattern env' (p, v)
                 | None -> None)
             with
             | Ok result -> result
             | Unequal_lengths -> None)))
    | ListPattern patterns, ListValue values
      when List.length patterns = List.length values ->
      let rec match_lists env pat_list val_list =
        match pat_list, val_list with
        | [], [] -> Some env
        | p :: ps, v :: vs ->
          (match match_pattern env (p, v) with
           | Some new_env -> match_lists new_env ps vs
           | None -> None)
        | _ -> None
      in
      match_lists env patterns values
    | OptionalPattern p, OptionalValue v ->
      (match p, v with
       | Some p, Some v -> match_pattern env (p, v)
       | None, None -> Some env
       | _ -> None)
    | _ -> None
  ;;

  let eval_unary = function
    | Negate, IntValue i -> return (IntValue (-i))
    | LogicalNot, BoolValue b -> return (BoolValue (not b))
    | _ -> fail TypeMismatch
  ;;

  let eval_binary (bop, v1, v2) =
    match bop, v1, v2 with
    | Multiply, IntValue x, IntValue y -> return (IntValue (x * y))
    | Divide, IntValue _, IntValue y when y = 0 -> fail DivisionByZero
    | Divide, IntValue x, IntValue y -> return (IntValue (x / y))
    | Add, IntValue x, IntValue y -> return (IntValue (x + y))
    | Subtract, IntValue x, IntValue y -> return (IntValue (x - y))
    | Equal, IntValue x, IntValue y -> return (BoolValue (x = y))
    | NotEqual, IntValue x, IntValue y -> return (BoolValue (x <> y))
    | Less, IntValue x, IntValue y -> return (BoolValue (x < y))
    | LessOrEqual, IntValue x, IntValue y -> return (BoolValue (x <= y))
    | Greater, IntValue x, IntValue y -> return (BoolValue (x > y))
    | GreaterOrEqual, IntValue x, IntValue y -> return (BoolValue (x >= y))
    | LogicalAnd, BoolValue x, BoolValue y -> return (BoolValue (x && y))
    | LogicalOr, BoolValue x, BoolValue y -> return (BoolValue (x || y))
    | _ -> fail TypeMismatch
  ;;

  let rec evaluate_expr env = function
    | LiteralExpr c ->
      (match c with
       | Integer i -> return (IntValue i)
       | Boolean b -> return (BoolValue b)
       | Text s -> return (StringValue s))
    | NameExpr x -> lookup env x
    | UnaryOpExpr (op, e) ->
      let* v = evaluate_expr env e in
      eval_unary (op, v)
    | BinaryOpExpr (op, e1, e2) ->
      let* v1 = evaluate_expr env e1 in
      let* v2 = evaluate_expr env e2 in
      eval_binary (op, v1, v2)
    | ConditionalExpr (cond, then_expr, else_expr_opt) ->
      let* cond_value = evaluate_expr env cond in
      (match cond_value with
       | BoolValue true -> evaluate_expr env then_expr
       | BoolValue false ->
         (match else_expr_opt with
          | Some else_expr -> evaluate_expr env else_expr
          | None -> return UnitValue)
       | _ -> fail TypeMismatch)
    | LetExpr (false, (ListPattern patterns, e1), _, e2) ->
      let check_list_pattern = function
        | NamePattern _ | WildcardPattern | UnitPattern
        | OptionalPattern (Some (NamePattern _)) -> true
        | _ -> false
      in
      if not (List.for_all patterns ~f:check_list_pattern)
      then fail InvalidPattern
      else
        let* v = evaluate_expr env e1 in
        (match match_pattern env (ListPattern patterns, v) with
         | Some env' -> evaluate_expr env' e2
         | None -> fail PatternMatchFailed)
    | LetExpr (false, (ProductPattern (p1, p2, rest), e1), _, e2) ->
      let check_product_pattern = function
        | NamePattern _ | WildcardPattern | UnitPattern
        | OptionalPattern (Some (NamePattern _)) -> true
        | _ -> false
      in
      if not (List.for_all ~f:check_product_pattern (p1 :: p2 :: rest))
      then fail InvalidPattern
      else
        let* v = evaluate_expr env e1 in
        (match match_pattern env (ProductPattern (p1, p2, rest), v) with
         | Some env' -> evaluate_expr env' e2
         | None -> fail PatternMatchFailed)
    | LetExpr (false, (pat, e1), _, e2) ->
      let check_simple_pattern =
        match pat with
        | WildcardPattern | NamePattern _ | UnitPattern
        | OptionalPattern (Some (NamePattern _)) -> true
        | _ -> false
      in
      if not check_simple_pattern
      then fail InvalidPattern
      else
        let* v = evaluate_expr env e1 in
        (match match_pattern env (pat, v) with
         | Some env' -> evaluate_expr env' e2
         | None -> fail PatternMatchFailed)
    | LetExpr (true, (pat, e1), [], e2) ->
      (match pat with
       | NamePattern _ ->
         let* v = evaluate_expr env e1 in
         let* rec_env =
           match match_pattern env (pat, v) with
           | Some new_env -> return new_env
           | None -> fail PatternMatchFailed
         in
         let* recursive_value =
           match v with
           | ClosureValue (_, p, pl, e, _) ->
             return (ClosureValue (true, p, pl, e, rec_env))
           | _ -> fail TypeMismatch
         in
         let* final_env =
           match match_pattern env (pat, recursive_value) with
           | Some updated_env -> return updated_env
           | None -> fail PatternMatchFailed
         in
         evaluate_expr final_env e2
       | _ -> fail InvalidPattern)
    | LetExpr (true, value_binding, value_bindings, e2) ->
      let bindings = List.map ~f:(fun (p, e) -> p, e) (value_binding :: value_bindings) in
      let rec update_env acc_env = function
        | [] -> return acc_env
        | (NamePattern name, expr) :: tl ->
          let* value =
            match expr with
            | LambdaExpr (patterns, e) ->
              let head = Option.value_exn (List.hd patterns) in
              let tail = Option.value_exn (List.tl patterns) in
              return (ClosureValue (true, head, tail, e, acc_env))
            | _ -> evaluate_expr acc_env expr
          in
          let updated_env = extend acc_env name value in
          update_env updated_env tl
        | _ -> fail InvalidPattern
      in
      let* final_env = update_env env bindings in
      evaluate_expr final_env e2
    | ProductExpr (e1, e2, es) ->
      let* v1 = evaluate_expr env e1 in
      let* v2 = evaluate_expr env e2 in
      let* vs =
        List.fold_right es ~init:(return []) ~f:(fun e acc ->
          let* acc = acc in
          let* v = evaluate_expr env e in
          return (v :: acc))
      in
      return (ProductValue (v1, v2, vs))
    | LambdaExpr (patterns, e) ->
      let head = Option.value_exn (List.hd patterns) in
      let tail = Option.value_exn (List.tl patterns) in
      return (ClosureValue (false, head, tail, e, env))
    | TypeAnnotationExpr (e, _) -> evaluate_expr env e
    | ApplyExpr (e1, e2) ->
      let* v1 = evaluate_expr env e1 in
      let* v2 = evaluate_expr env e2 in
      (match v1 with
       | BuiltinFunction f ->
         (match f v2 with
          | Ok result -> return result
          | Error err -> fail err)
       | ClosureValue (_, pat, pats, body, func_env) ->
         (match match_pattern func_env (pat, v2) with
          | Some extended_env ->
            let env' =
              Map.fold extended_env ~init:env ~f:(fun ~key ~data acc_env ->
                Map.update acc_env key ~f:(fun _ -> data))
            in
            (match pats with
             | [] -> evaluate_expr env' body
             | p :: pl -> return (ClosureValue (false, p, pl, body, env')))
          | None -> fail PatternMatchFailed)
       | _ -> fail TypeMismatch)
    | ListExpr el ->
      let rec eval_list_elements env = function
        | [] -> return []
        | e :: es ->
          let* v = evaluate_expr env e in
          let* vs = eval_list_elements env es in
          return (v :: vs)
      in
      let* vl = eval_list_elements env el in
      return (ListValue vl)
    | OptionalExpr opt ->
      let* value =
        match opt with
        | Some expr ->
          let* v = evaluate_expr env expr in
          return (Some v)
        | None -> return None
      in
      return (OptionalValue value)
  ;;

  let evaluate_declaration env = function
    | ExprDeclaration expr ->
      let* _ = evaluate_expr env expr in
      return env
    | BindingDeclaration (false, (ListPattern patterns, e), _) ->
      let check_list_pattern = function
        | NamePattern _ | WildcardPattern | UnitPattern
        | OptionalPattern (Some (NamePattern _)) -> true
        | _ -> false
      in
      if not (List.for_all ~f:check_list_pattern patterns)
      then fail InvalidPattern
      else
        let* v = evaluate_expr env e in
        (match match_pattern env (ListPattern patterns, v) with
         | Some env' -> return env'
         | None -> fail PatternMatchFailed)
    | BindingDeclaration (false, (ProductPattern (p1, p2, rest), e), _) ->
      let check_product_pattern = function
        | NamePattern _ | WildcardPattern | UnitPattern
        | OptionalPattern (Some (NamePattern _)) -> true
        | _ -> false
      in
      if not (List.for_all ~f:check_product_pattern (p1 :: p2 :: rest))
      then fail InvalidPattern
      else
        let* v = evaluate_expr env e in
        (match match_pattern env (ProductPattern (p1, p2, rest), v) with
         | Some env' -> return env'
         | None -> fail PatternMatchFailed)
    | BindingDeclaration (false, (pattern, expr), _) ->
      let check_simple_pattern =
        match pattern with
        | WildcardPattern | NamePattern _ | UnitPattern
        | OptionalPattern (Some (NamePattern _)) -> true
        | _ -> false
      in
      if not check_simple_pattern
      then fail InvalidPattern
      else
        let* v = evaluate_expr env expr in
        (match match_pattern env (pattern, v) with
         | Some env' -> return env'
         | None -> fail PatternMatchFailed)
    | BindingDeclaration (true, ((NamePattern _ as pattern), expr), []) ->
      let* v = evaluate_expr env expr in
      let* rec_env =
        match match_pattern env (pattern, v) with
        | Some new_env -> return new_env
        | None -> fail PatternMatchFailed
      in
      let* recursive_value =
        match v with
        | ClosureValue (_, p, pl, expr, _) ->
          return (ClosureValue (true, p, pl, expr, rec_env))
        | _ -> fail TypeMismatch
      in
      let* final_env =
        match match_pattern env (pattern, recursive_value) with
        | Some updated_env -> return updated_env
        | None -> fail PatternMatchFailed
      in
      return final_env
    | BindingDeclaration (true, _, []) -> fail InvalidPattern
    | BindingDeclaration (true, value_binding, value_bindings) ->
      let bindings = value_binding :: value_bindings in
      let rec update_env acc_env = function
        | [] -> return acc_env
        | (NamePattern name, expr) :: tl ->
          let* value =
            match expr with
            | LambdaExpr (patterns, expr) ->
              let head = Option.value_exn (List.hd patterns) in
              let tail = Option.value_exn (List.tl patterns) in
              return (ClosureValue (true, head, tail, expr, acc_env))
            | _ -> evaluate_expr acc_env expr
          in
          let updated_env = extend acc_env name value in
          update_env updated_env tl
        | _ -> fail InvalidPattern
      in
      let* final_env = update_env env bindings in
      return final_env
  ;;

  let evaluate_script script =
    List.fold_left script ~init:(return initial_env) ~f:(fun env decl ->
      let* env = env in
      let* env = evaluate_declaration env decl in
      return env)
  ;;
end

module ResultMonad = struct
  type ('a, 'e) monad = ('a, 'e) Result.t

  let return = Result.return
  let fail = Result.fail
  let ( let* ) m f = Result.bind m ~f
end

module Eval = Evaluator (ResultMonad)
