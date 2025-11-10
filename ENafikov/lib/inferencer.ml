(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Stdlib.Format

type type_error =
  | CircularCheck of int * type_expr
  | UnknownName of string
  | TypeMismatch of type_expr * type_expr
  | MultipleBounds of string
  | PatternError of string
  | ExpressionError of string
  | UnexpectedArrow of type_expr

let print_error fmt = function
  | CircularCheck (id, ty) ->
    fprintf
      fmt
      "CircularCheck failed. Type variable '%d occurs inside %a."
      id
      print_type
      ty
  | UnknownName name -> fprintf fmt "Unknown name '%s'." name
  | TypeMismatch (ty1, ty2) ->
    fprintf fmt "TypeMismatch: %a and %a." print_type ty1 print_type ty2
  | MultipleBounds name -> fprintf fmt "MultipleBounds for '%s'." name
  | PatternError msg -> fprintf fmt "PatternError: %s." msg
  | ExpressionError msg -> fprintf fmt "ExpressionError: %s." msg
  | UnexpectedArrow ty1 -> fprintf fmt "UnexpectedArrow: %a" print_type ty1
;;

module IntSet = struct
  include Stdlib.Set.Make (Int)
end

module InferenceMonad : sig
  type 'a computation

  val pure : 'a -> 'a computation
  val throw : type_error -> 'a computation

  include Monad.Infix with type 'a t := 'a computation

  module Syntax : sig
    val ( let* ) : 'a computation -> ('a -> 'b computation) -> 'b computation
  end

  val new_var : int computation
  val execute : 'a computation -> ('a, type_error) Result.t

  module TypeMap : sig
    val fold
      :  ('a, 'b, 'c) Map.t
      -> init:'d computation
      -> f:('a -> 'b -> 'd -> 'd computation)
      -> 'd computation
  end
end = struct
  type 'a computation = int -> int * ('a, type_error) Result.t

  let ( >>= ) m f state =
    let last, r = m state in
    match r with
    | Result.Error x -> last, Result.fail x
    | Result.Ok a -> f a last
  ;;

  let pure x last = last, Result.return x
  let throw e st = st, Result.fail e

  let ( >>| ) m f st =
    match m st with
    | st, Ok x -> st, Result.return (f x)
    | st, Result.Error e -> st, Result.fail e
  ;;

  module Syntax = struct
    let ( let* ) = ( >>= )
  end

  module TypeMap = struct
    let fold map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let new_var : int computation = fun last -> last + 1, Result.return last
  let execute monad = snd (monad 0)
end

module TypeOps = struct
  let rec occurs var = function
    | TypeVar b -> b = var
    | FuncType (left, right) -> occurs var left || occurs var right
    | ProductType types -> List.exists types ~f:(occurs var)
    | ListType ty -> occurs var ty
    | OptionalType ty -> occurs var ty
    | BaseType _ -> false
  ;;

  let free_vars =
    let rec collect acc = function
      | TypeVar b -> IntSet.add b acc
      | FuncType (left, right) -> collect (collect acc left) right
      | ProductType types -> List.fold_left types ~init:acc ~f:collect
      | ListType ty -> collect acc ty
      | OptionalType ty -> collect acc ty
      | BaseType _ -> acc
    in
    collect IntSet.empty
  ;;
end

module TypeSubst : sig
  type substitution

  val empty : substitution
  val single : int -> type_expr -> substitution InferenceMonad.computation
  val remove : substitution -> int -> substitution
  val apply_subst : substitution -> type_expr -> type_expr
  val unify : type_expr -> type_expr -> substitution InferenceMonad.computation
  val combine : substitution -> substitution -> substitution InferenceMonad.computation
  val combine_all : substitution list -> substitution InferenceMonad.computation
end = struct
  open InferenceMonad
  open InferenceMonad.Syntax

  type substitution = (int, type_expr, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let create_mapping key value =
    if TypeOps.occurs key value
    then throw (CircularCheck (key, value))
    else pure (key, value)
  ;;

  let single key value =
    let* key, value = create_mapping key value in
    pure (Map.singleton (module Int) key value)
  ;;

  let find = Map.find
  let remove = Map.remove

  let apply_subst subst =
    let rec apply = function
      | BaseType x -> BaseType x
      | TypeVar b as ty ->
        (match find subst b with
         | None -> ty
         | Some x -> x)
      | FuncType (left, right) -> FuncType (apply left, apply right)
      | ListType ty -> ListType (apply ty)
      | OptionalType ty -> OptionalType (apply ty)
      | ProductType types -> ProductType (List.map ~f:apply types)
    in
    apply
  ;;

  let rec unify left right =
    match left, right with
    | BaseType l, BaseType r when String.equal l r -> pure empty
    | BaseType _, BaseType _ -> throw (TypeMismatch (left, right))
    | TypeVar l, TypeVar r when l = r -> pure empty
    | TypeVar b, ty | ty, TypeVar b -> single b ty
    | FuncType (left1, right1), FuncType (left2, right2) ->
      let* subst1 = unify left1 left2 in
      let* subst2 = unify (apply_subst subst1 right1) (apply_subst subst1 right2) in
      combine subst1 subst2
    | ProductType types1, ProductType types2 ->
      if List.length types1 <> List.length types2
      then throw (TypeMismatch (left, right))
      else (
        let rec unify_products subst types1 types2 =
          match types1, types2 with
          | [], [] -> pure subst
          | t1 :: rest1, t2 :: rest2 ->
            let* subst' = unify (apply_subst subst t1) (apply_subst subst t2) in
            let* combined_subst = combine subst subst' in
            unify_products combined_subst rest1 rest2
          | _, _ -> throw (TypeMismatch (left, right))
        in
        unify_products empty types1 types2)
    | ListType ty1, ListType ty2 -> unify ty1 ty2
    | OptionalType ty1, OptionalType ty2 -> unify ty1 ty2
    | _ -> throw (TypeMismatch (left, right))

  and extend key value subst =
    match find subst key with
    | None ->
      let value = apply_subst subst value in
      let* subst2 = single key value in
      TypeMap.fold subst ~init:(pure subst2) ~f:(fun key value acc ->
        let value = apply_subst subst2 value in
        let* key, value = create_mapping key value in
        pure (Map.update acc key ~f:(fun _ -> value)))
    | Some value2 ->
      let* subst2 = unify value value2 in
      combine subst subst2

  and combine subst1 subst2 = TypeMap.fold subst2 ~init:(pure subst1) ~f:extend

  let combine_all =
    List.fold_left ~init:(pure empty) ~f:(fun acc subst ->
      let* acc = acc in
      combine acc subst)
  ;;
end

module TypeScheme = struct
  type scheme = Forall of IntSet.t * type_expr

  let free_vars (Forall (vars, ty)) = IntSet.diff (TypeOps.free_vars ty) vars

  let apply subst (Forall (vars, ty)) =
    let subst2 = IntSet.fold (fun key subst -> TypeSubst.remove subst key) vars subst in
    Forall (vars, TypeSubst.apply_subst subst2 ty)
  ;;
end

module Context = struct
  type context = (name, TypeScheme.scheme, String.comparator_witness) Map.t

  let add ctx key value = Map.update ctx key ~f:(fun _ -> value)

  let free_vars : context -> IntSet.t =
    Map.fold ~init:IntSet.empty ~f:(fun ~key:_ ~data:scheme acc ->
      IntSet.union acc (TypeScheme.free_vars scheme))
  ;;

  let apply subst ctx = Map.map ctx ~f:(TypeScheme.apply subst)
  let lookup = Map.find

  let initial_context =
    let open Base.Map in
    empty (module String)
    |> set
         ~key:"print_int"
         ~data:
           (TypeScheme.Forall (IntSet.empty, FuncType (BaseType "int", BaseType "unit")))
    |> set
         ~key:"print_endline"
         ~data:
           (TypeScheme.Forall (IntSet.empty, FuncType (BaseType "string", BaseType "unit")))
    |> set
         ~key:"print_bool"
         ~data:
           (TypeScheme.Forall (IntSet.empty, FuncType (BaseType "bool", BaseType "unit")))
  ;;
end

open InferenceMonad
open InferenceMonad.Syntax

let fresh_type_var = new_var >>| fun n -> TypeVar n

let instantiate : TypeScheme.scheme -> type_expr InferenceMonad.computation =
  fun (Forall (vars, ty)) ->
  IntSet.fold
    (fun var typ ->
      let* typ = typ in
      let* fresh_ty = fresh_type_var in
      let* subst = TypeSubst.single var fresh_ty in
      pure (TypeSubst.apply_subst subst typ))
    vars
    (pure ty)
;;

let generalize ctx ty =
  let free = IntSet.diff (TypeOps.free_vars ty) (Context.free_vars ctx) in
  TypeScheme.Forall (free, ty)
;;

let type_of_literal = function
  | Integer _ -> BaseType "int"
  | Boolean _ -> BaseType "bool"
  | Text _ -> BaseType "string"
;;

let rec infer_pattern ctx = function
  | WildcardPattern ->
    let* fresh = fresh_type_var in
    pure (TypeSubst.empty, fresh, ctx)
  | LiteralPattern lit -> pure (TypeSubst.empty, type_of_literal lit, ctx)
  | NamePattern var ->
    let* fresh = fresh_type_var in
    let ctx = Context.add ctx var (TypeScheme.Forall (IntSet.empty, fresh)) in
    pure (TypeSubst.empty, fresh, ctx)
  | ProductPattern (first_pat, second_pat, rest_pats) ->
    let* sub_first, type_first, ctx_first = infer_pattern ctx first_pat in
    let updated_ctx_second = Context.apply sub_first ctx_first in
    let* sub_second, type_second, ctx_second =
      infer_pattern updated_ctx_second second_pat
    in
    let process_remaining_patterns acc pat =
      let open InferenceMonad.Syntax in
      let* current_sub, types, current_ctx = acc in
      let* sub_new, type_new, ctx_new = infer_pattern current_ctx pat in
      let* combined_sub = TypeSubst.combine current_sub sub_new in
      pure (combined_sub, type_new :: types, ctx_new)
    in
    let initial_state = pure (sub_second, [ type_second; type_first ], ctx_second) in
    let* final_sub, collected_types, final_ctx =
      List.fold_left rest_pats ~init:initial_state ~f:process_remaining_patterns
    in
    let product_type = ProductType (List.rev collected_types) in
    pure (final_sub, product_type, final_ctx)
  | ListPattern pats ->
    let* fresh_el_type = fresh_type_var in
    let* final_sub, final_ctx =
      List.fold_left
        pats
        ~init:(pure (TypeSubst.empty, ctx))
        ~f:(fun acc pat ->
          let open InferenceMonad.Syntax in
          let* sub_acc, ctx_acc = acc in
          let* sub_cur, el_type, ctx_cur = infer_pattern ctx_acc pat in
          let* unified_sub = TypeSubst.combine sub_acc sub_cur in
          let* final_sub =
            TypeSubst.unify (TypeSubst.apply_subst sub_cur fresh_el_type) el_type
          in
          let* combined_sub = TypeSubst.combine unified_sub final_sub in
          pure (combined_sub, Context.apply final_sub ctx_cur))
    in
    pure (final_sub, ListType (TypeSubst.apply_subst final_sub fresh_el_type), final_ctx)
  | OptionalPattern opt ->
    let* sub, typ, ctx =
      match opt with
      | None ->
        let* fresh = fresh_type_var in
        pure (TypeSubst.empty, fresh, ctx)
      | Some p -> infer_pattern ctx p
    in
    pure (sub, OptionalType typ, ctx)
  | AnnotatedPattern (pat, annotated_ty) ->
    let* subst, inferred_ty, ctx = infer_pattern ctx pat in
    let* unified_subst = TypeSubst.unify inferred_ty annotated_ty in
    let* total_subst = TypeSubst.combine subst unified_subst in
    pure
      ( total_subst
      , TypeSubst.apply_subst total_subst annotated_ty
      , Context.apply total_subst ctx )
  | UnitPattern -> pure (TypeSubst.empty, BaseType "unit", ctx)
;;

let type_of_binop = function
  | Equal | NotEqual | Greater | GreaterOrEqual | Less | LessOrEqual ->
    fresh_type_var >>| fun fresh_ty -> fresh_ty, fresh_ty, BaseType "bool"
  | Add | Subtract | Multiply | Divide ->
    pure (BaseType "int", BaseType "int", BaseType "int")
  | LogicalAnd | LogicalOr -> pure (BaseType "bool", BaseType "bool", BaseType "bool")
;;

let rec infer_expr ctx = function
  | LiteralExpr lit -> pure (TypeSubst.empty, type_of_literal lit)
  | NameExpr var ->
    (match Context.lookup ctx var with
     | Some scheme ->
       let* ty = instantiate scheme in
       pure (TypeSubst.empty, ty)
     | None -> throw (UnknownName var))
  | UnaryOpExpr (operation, expr) ->
    let* subst, ty = infer_expr ctx expr in
    let* operation_type =
      match operation with
      | Negate -> pure (FuncType (BaseType "int", BaseType "int"))
      | LogicalNot -> pure (FuncType (BaseType "bool", BaseType "bool"))
    in
    let* subst2 =
      match operation_type with
      | FuncType (arg, _) -> TypeSubst.unify ty arg
      | ty -> throw (UnexpectedArrow ty)
    in
    let* subst2 = TypeSubst.combine_all [ subst2; subst ] in
    (match operation_type with
     | FuncType (_, x) -> pure (subst2, TypeSubst.apply_subst subst2 x)
     | ty -> throw (UnexpectedArrow ty))
  | BinaryOpExpr (op, expr1, expr2) ->
    let* subst1, ty = infer_expr ctx expr1 in
    let* subst2, ty' = infer_expr (Context.apply subst1 ctx) expr2 in
    let* ty1_op, ty2_op, ty_res = type_of_binop op in
    let* subst3 = TypeSubst.unify (TypeSubst.apply_subst subst2 ty) ty1_op in
    let* subst4 = TypeSubst.unify (TypeSubst.apply_subst subst3 ty') ty2_op in
    let* subst = TypeSubst.combine_all [ subst1; subst2; subst3; subst4 ] in
    pure (subst, TypeSubst.apply_subst subst ty_res)
  | ConditionalExpr (cond, then_expr, else_expr) ->
    let* subst1, ty1 = infer_expr ctx cond in
    let* subst2, ty2 = infer_expr (Context.apply subst1 ctx) then_expr in
    let* ty3 =
      match else_expr with
      | Some el ->
        let* _, ty3 = infer_expr (Context.apply subst2 ctx) el in
        pure ty3
      | None -> pure (BaseType "unit")
    in
    let* subst4 = TypeSubst.unify ty1 (BaseType "bool") in
    let* subst5 = TypeSubst.unify ty2 ty3 in
    let* total_subst =
      match else_expr with
      | Some el ->
        let* subst3, _ = infer_expr (Context.apply subst2 ctx) el in
        TypeSubst.combine_all [ subst5; subst4; subst3; subst2; subst1 ]
      | None -> TypeSubst.combine_all [ subst5; subst4; subst2; subst1 ]
    in
    pure (total_subst, TypeSubst.apply_subst total_subst ty2)
  | ProductExpr (expr1, expr2, exprs) ->
    let* subst1, ty1 = infer_expr ctx expr1 in
    let* subst2, ty2 = infer_expr (Context.apply subst1 ctx) expr2 in
    let infer_product_elements ctx es =
      let rec aux ctx = function
        | [] -> pure ([], [])
        | e :: es' ->
          let* s, t = infer_expr ctx e in
          let* s', ts = aux (Context.apply s ctx) es' in
          pure (s' @ [ s ], t :: ts)
      in
      aux ctx es
    in
    let* subst3, tys = infer_product_elements (Context.apply subst2 ctx) exprs in
    let* subst = TypeSubst.combine_all (subst3 @ [ subst2; subst1 ]) in
    pure (subst, ProductType (ty1 :: ty2 :: tys))
  | ListExpr exprs ->
    (match exprs with
     | [] ->
       let* fresh = fresh_type_var in
       pure (TypeSubst.empty, ListType fresh)
     | _ :: _ ->
       let infer_list_elements ctx es =
         let rec aux ctx = function
           | [] -> pure ([], [])
           | e :: es' ->
             let* s, t = infer_expr ctx e in
             let* s', ts = aux (Context.apply s ctx) es' in
             pure (s' @ [ s ], t :: ts)
         in
         aux ctx es
       in
       let* subst, tys = infer_list_elements ctx exprs in
       let* total_subst = TypeSubst.combine_all subst in
       (match tys with
        | [] -> throw (MultipleBounds "inferred empty list type")
        | ty :: _ -> pure (total_subst, ListType ty)))
  | LetExpr (false, (NamePattern x, expr1), _, expr2) ->
    let* subst1, ty1 = infer_expr ctx expr1 in
    let ctx2 = Context.apply subst1 ctx in
    let ty_gen = generalize ctx2 ty1 in
    let ctx3 = Context.add ctx x ty_gen in
    let* subst2, ty2 = infer_expr (Context.apply subst1 ctx3) expr2 in
    let* total_subst = TypeSubst.combine subst1 subst2 in
    pure (total_subst, ty2)
  | LetExpr (false, (pattern, expr1), bindings, expr2) ->
    let* subst1, ty1 = infer_expr ctx expr1 in
    let* subst2, ty_pat, ctx1 = infer_pattern ctx pattern in
    let* subst = TypeSubst.combine subst1 subst2 in
    let* unified_subst = TypeSubst.unify (TypeSubst.apply_subst subst ty_pat) ty1 in
    let initial_ctx = Context.apply unified_subst ctx1 in
    let* extended_ctx =
      List.fold_left
        ~f:(fun acc_ctx (pattern, expr) ->
          let* acc_ctx = acc_ctx in
          let* subst_bind, ty_bind = infer_expr acc_ctx expr in
          let* subst_pattern, _, ctx_pattern = infer_pattern acc_ctx pattern in
          let* combined_subst = TypeSubst.combine subst_bind subst_pattern in
          let* final_subst =
            TypeSubst.unify (TypeSubst.apply_subst combined_subst ty_pat) ty_bind
          in
          let updated_ctx =
            Map.fold
              ~init:(Context.apply final_subst acc_ctx)
              ~f:(fun ~key ~data acc_ctx -> Context.add acc_ctx key data)
              (Context.apply final_subst ctx_pattern)
          in
          pure updated_ctx)
        ~init:(pure initial_ctx)
        bindings
    in
    let* subst3, ty2 = infer_expr extended_ctx expr2 in
    let* total_subst = TypeSubst.combine_all [ subst3; unified_subst; subst ] in
    pure (total_subst, ty2)
  | LetExpr (true, (NamePattern x, expr1), [], expr2) ->
    let* expr1 =
      match expr1 with
      | LambdaExpr _ -> pure expr1
      | _ ->
        throw (ExpressionError "Right-hand side of let rec must be a lambda expression")
    in
    let* tv = fresh_type_var in
    let ctx2 = Context.add ctx x (TypeScheme.Forall (IntSet.empty, tv)) in
    let* subst1, ty1 = infer_expr ctx2 expr1 in
    let* subst2 = TypeSubst.unify (TypeSubst.apply_subst subst1 tv) ty1 in
    let* subst_total = TypeSubst.combine subst1 subst2 in
    let ctx3 = Context.apply subst_total ctx in
    let ctx4 = Context.apply subst1 ctx3 in
    let ty_gen = generalize ctx4 (TypeSubst.apply_subst subst_total tv) in
    let* subst3, ty2 = infer_expr (Context.add ctx4 x ty_gen) expr2 in
    let* subst_total = TypeSubst.combine subst_total subst3 in
    pure (subst_total, ty2)
  | LetExpr (true, value_binding, value_bindings, expr2) ->
    let* ctx_ext, subst_acc =
      List.fold_left
        ~f:(fun acc_ctx (pat, expr) ->
          let* expr =
            match expr with
            | LambdaExpr _ -> pure expr
            | _ ->
              throw
                (ExpressionError "Right-hand side of let rec must be a lambda expression")
          in
          let* pat =
            match pat with
            | NamePattern _ -> pure pat
            | _ ->
              throw
                (PatternError
                   "Only variables are allowed on the left-hand side of let rec")
          in
          let* ctx_acc, _ = acc_ctx in
          let* subst_expr, ty_expr = infer_expr ctx_acc expr in
          let* subst_pattern, ty_pat, ctx_pat = infer_pattern ctx_acc pat in
          let* subst = TypeSubst.combine subst_expr subst_pattern in
          let* unified_subst = TypeSubst.unify ty_expr ty_pat in
          let* combined_subst = TypeSubst.combine subst unified_subst in
          let extended_ctx = Context.apply combined_subst ctx_pat in
          pure (extended_ctx, combined_subst))
        ~init:(pure (ctx, TypeSubst.empty))
        (value_binding :: value_bindings)
    in
    let* subst2, ty2 = infer_expr ctx_ext expr2 in
    let* total_subst = TypeSubst.combine subst_acc subst2 in
    pure (total_subst, ty2)
  | LambdaExpr (patterns, body) ->
    let* ctx, pat_types =
      List.fold_left
        patterns
        ~init:(pure (ctx, []))
        ~f:(fun acc pat ->
          let* ctx, pat_types = acc in
          let* _, typ, ctx = infer_pattern ctx pat in
          pure (ctx, typ :: pat_types))
    in
    let* subst_body, ty_body = infer_expr ctx body in
    let arrow_type =
      List.fold_right
        ~f:(fun pat_type acc -> FuncType (TypeSubst.apply_subst subst_body pat_type, acc))
        ~init:ty_body
        (List.rev pat_types)
    in
    pure (subst_body, arrow_type)
  | ApplyExpr (param, body) ->
    let* subst1, ty1 = infer_expr ctx param in
    let* subst2, ty2 = infer_expr (Context.apply subst1 ctx) body in
    let* tv = fresh_type_var in
    let* subst3 =
      TypeSubst.unify (TypeSubst.apply_subst subst2 ty1) (FuncType (ty2, tv))
    in
    let* total_subst = TypeSubst.combine_all [ subst3; subst2; subst1 ] in
    pure (total_subst, TypeSubst.apply_subst total_subst tv)
  | OptionalExpr opt_expr ->
    (match opt_expr with
     | Some expr ->
       let* subst, ty = infer_expr ctx expr in
       pure (subst, OptionalType ty)
     | None ->
       let* tv = fresh_type_var in
       pure (TypeSubst.empty, OptionalType tv))
  | TypeAnnotationExpr (expr, t) ->
    let* subst1, ty1 = infer_expr ctx expr in
    let* subst2 = TypeSubst.unify ty1 (TypeSubst.apply_subst subst1 t) in
    let* total_subst = TypeSubst.combine subst1 subst2 in
    pure (total_subst, TypeSubst.apply_subst subst2 ty1)
;;

let infer_declaration ctx = function
  | ExprDeclaration expr ->
    let* subst, _ = infer_expr ctx expr in
    let updated_ctx = Context.apply subst ctx in
    pure (subst, updated_ctx)
  | BindingDeclaration (true, (NamePattern x, expr), []) ->
    let* expr =
      match expr with
      | LambdaExpr _ -> pure expr
      | _ ->
        throw (ExpressionError "Right-hand side of let rec must be a lambda expression")
    in
    let* tv = fresh_type_var in
    let ctx = Context.add ctx x (TypeScheme.Forall (IntSet.empty, tv)) in
    let* subst, ty = infer_expr ctx expr in
    let* subst2 = TypeSubst.unify (TypeSubst.apply_subst subst tv) ty in
    let* composed_subst = TypeSubst.combine subst subst2 in
    let ctx2 = Context.apply composed_subst ctx in
    let generalized_ty = generalize ctx2 (TypeSubst.apply_subst composed_subst ty) in
    let ctx = Context.add ctx2 x generalized_ty in
    pure (composed_subst, ctx)
  | BindingDeclaration (true, value_binding, value_bindings) ->
    let all_bindings = value_binding :: value_bindings in
    let* ctx_with_placeholders =
      List.fold_left
        ~f:(fun acc_ctx (pat, _) ->
          let* pat =
            match pat with
            | NamePattern _ -> pure pat
            | _ ->
              throw
                (PatternError
                   "Only variables are allowed on the left-hand side of let rec")
          in
          let* ctx_acc = acc_ctx in
          let* subst_pat, _, ctx_pat = infer_pattern ctx_acc pat in
          let extended_ctx = Context.apply subst_pat ctx_pat in
          pure extended_ctx)
        ~init:(pure ctx)
        all_bindings
    in
    let* ctx_ext, subst_acc =
      List.fold_left
        ~f:(fun acc_ctx (ty_pattern, expr) ->
          let* expr =
            match expr with
            | LambdaExpr _ -> pure expr
            | _ ->
              throw
                (ExpressionError "Right-hand side of let rec must be a lambda expression")
          in
          let* ctx_acc, _ = acc_ctx in
          let* subst_expr, ty_expr = infer_expr ctx_acc expr in
          let* subst_pat, ty_pat, ctx_pat = infer_pattern ctx_acc ty_pattern in
          let* subst = TypeSubst.combine subst_expr subst_pat in
          let* unified_subst = TypeSubst.unify ty_expr ty_pat in
          let* combined_subst = TypeSubst.combine subst unified_subst in
          let extended_ctx = Context.apply combined_subst ctx_pat in
          pure (extended_ctx, combined_subst))
        ~init:(pure (ctx_with_placeholders, TypeSubst.empty))
        all_bindings
    in
    pure (subst_acc, ctx_ext)
  | BindingDeclaration (false, (NamePattern x, expr), _) ->
    let* subst, ty = infer_expr ctx expr in
    let ctx2 = Context.apply subst ctx in
    let generalized_ty = generalize ctx2 ty in
    let ctx = Context.add (Context.apply subst ctx) x generalized_ty in
    pure (subst, ctx)
  | BindingDeclaration (false, (pattern, expr), _) ->
    let* subst_expr, ty = infer_expr ctx expr in
    let* subst_pat, ty_pat, ctx_pat = infer_pattern ctx pattern in
    let* combined_subst = TypeSubst.combine subst_expr subst_pat in
    let* unified_subst =
      TypeSubst.unify (TypeSubst.apply_subst combined_subst ty_pat) ty
    in
    let updated_ctx = Context.apply unified_subst ctx_pat in
    let* final_subst = TypeSubst.combine unified_subst combined_subst in
    pure (final_subst, updated_ctx)
;;

let infer_script ctx script =
  let rec process_script ctx subst = function
    | [] -> pure (subst, ctx)
    | item :: rest ->
      let* subst1, ctx1 = infer_declaration ctx item in
      let* composed_subst = TypeSubst.combine subst subst1 in
      process_script ctx1 composed_subst rest
  in
  process_script ctx TypeSubst.empty script
;;

let infer_simple_expr expr =
  Result.map ~f:snd (execute (infer_expr Context.initial_context expr))
;;

let run_inference str =
  Result.map ~f:snd (execute (infer_script Context.initial_context str))
;;
