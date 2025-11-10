(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Angstrom

let is_reserved = function
  | "let"
  | "match"
  | "in"
  | "if"
  | "then"
  | "else"
  | "fun"
  | "rec"
  | "true"
  | "false"
  | "Some"
  | "and" -> true
  | _ -> false
;;

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let whitespace = take_while Char.is_whitespace
let symbol s = whitespace *> string s
let symbol1 s = whitespace *> s
let parenthesized p = symbol "(" *> p <* symbol ")"

let parse_int_lit =
  let sign = choice [ symbol "" ] in
  let num = take_while1 Char.is_digit in
  lift2 (fun s n -> Integer (Int.of_string (s ^ n))) sign num
;;

let parse_bool_lit =
  choice
    [ symbol "true" *> return (Boolean true); symbol "false" *> return (Boolean false) ]
;;

let parse_string_lit =
  symbol "\"" *> take_till (Char.equal '\"') <* symbol "\"" >>| fun s -> Text s
;;

let parse_literal = choice [ parse_int_lit; parse_bool_lit; parse_string_lit ]

let parse_unary_op =
  choice [ symbol "-" *> return Negate; symbol "not" *> return LogicalNot ]
;;

let parse_name =
  let first_char =
    satisfy (fun ch -> is_lowercase ch || is_uppercase ch || Char.equal ch '_')
    >>| Char.escaped
  in
  let other_chars =
    take_while (fun ch ->
      is_lowercase ch || is_uppercase ch || is_digit ch || Char.equal ch '_')
  in
  symbol1 @@ lift2 ( ^ ) first_char other_chars
  >>= fun s -> if is_reserved s then fail "Not a valid name" else return s
;;

let parse_base_type =
  choice
    [ symbol "int" *> return (BaseType "int")
    ; symbol "bool" *> return (BaseType "bool")
    ; symbol "string" *> return (BaseType "string")
    ; symbol "unit" *> return (BaseType "unit")
    ]
;;

let rec parse_type_list t =
  let* base = t in
  whitespace
  *> symbol "list"
  *> (parse_type_list (return (ListType base)) <|> return (ListType base))
;;

let parse_type_expr =
  let base_type = parse_base_type in
  let list_type = parse_type_list base_type <|> base_type in
  list_type
;;

let parse_annotated_pattern parse_pattern =
  let* pat = whitespace *> symbol "(" *> parse_pattern in
  let* constr =
    whitespace *> symbol ":" *> whitespace *> parse_type_expr <* whitespace <* symbol ")"
  in
  return (AnnotatedPattern (pat, constr))
;;

let parse_name_pattern = parse_name >>| fun id -> NamePattern id
let parse_literal_pattern = parse_literal >>| fun c -> LiteralPattern c
let parse_wildcard_pattern = symbol "_" *> return WildcardPattern

let parse_product_pattern parse_pattern =
  let parse_unparenthesized =
    lift3
      (fun p1 p2 rest -> ProductPattern (p1, p2, rest))
      parse_pattern
      (symbol "," *> parse_pattern)
      (many (symbol "," *> parse_pattern))
    <* whitespace
  in
  parenthesized parse_unparenthesized <|> parse_unparenthesized
;;

let parse_list_pattern parse_pattern =
  let semicols = symbol ";" in
  symbol "[" *> (sep_by semicols parse_pattern >>| fun patterns -> ListPattern patterns)
  <* symbol "]"
;;

let parse_unit_pattern = symbol "()" *> return UnitPattern

let parse_optional_pattern parse_pattern =
  lift
    (fun e -> OptionalPattern e)
    (symbol "Some" *> parse_pattern
     >>| (fun e -> Some e)
     <|> (symbol "None" >>| fun _ -> None))
;;

let parse_pattern =
  fix (fun pat ->
    let atom =
      choice
        [ parse_name_pattern
        ; parse_wildcard_pattern
        ; parse_literal_pattern
        ; parse_unit_pattern
        ; parse_annotated_pattern pat
        ; parenthesized pat
        ; parse_optional_pattern pat
        ]
    in
    let tuple = parse_product_pattern atom <|> atom in
    let lst = parse_list_pattern tuple <|> tuple in
    lst)
;;

let parse_left_associative expr oper =
  let rec go acc = lift2 (fun f x -> f acc x) oper expr >>= go <|> return acc in
  expr >>= go
;;

let parse_binary_op parse_bin_op tkn =
  symbol tkn *> return (fun e1 e2 -> BinaryOpExpr (parse_bin_op, e1, e2))
;;

let multiply = parse_binary_op Multiply "*"
let divide = parse_binary_op Divide "/"
let add = parse_binary_op Add "+"
let subtract = parse_binary_op Subtract "-"

let compare_ops =
  choice
    [ parse_binary_op Equal "="
    ; parse_binary_op NotEqual "<>"
    ; parse_binary_op LessOrEqual "<="
    ; parse_binary_op Less "<"
    ; parse_binary_op GreaterOrEqual ">="
    ; parse_binary_op Greater ">"
    ]
;;

let and_op = parse_binary_op LogicalAnd "&&"
let or_op = parse_binary_op LogicalOr "||"
let parse_name_expr = parse_name >>| fun x -> NameExpr x
let parse_literal_expr = parse_literal >>| fun c -> LiteralExpr c

let parse_annotated_expr parse_expr =
  let parse_type_annotation = symbol ":" *> parse_type_expr in
  lift2 (fun expr t -> TypeAnnotationExpr (expr, t)) parse_expr parse_type_annotation
;;

let parse_conditional_expr parse_expr =
  lift3
    (fun cond t f -> ConditionalExpr (cond, t, f))
    (symbol "if" *> parse_expr)
    (symbol "then" *> parse_expr)
    (option None (symbol "else" *> parse_expr >>| Option.some))
;;

let parse_optional_expr parse_expr =
  choice
    [ symbol "None" *> return (OptionalExpr None)
    ; (symbol "Some" *> choice [ parenthesized parse_expr; parse_expr ]
       >>| fun e -> OptionalExpr (Some e))
    ]
;;

let parse_unary_op_expr parse_expr =
  parse_unary_op >>= fun op -> parse_expr >>= fun expr -> return (UnaryOpExpr (op, expr))
;;

let parse_list_expr parse_expr =
  let parse_elements = sep_by (symbol ";") parse_expr in
  symbol "[" *> parse_elements <* symbol "]" >>| fun elements -> ListExpr elements
;;

let parse_application_expr e =
  parse_left_associative e (return (fun e1 e2 -> ApplyExpr (e1, e2)))
;;

let parse_lambda_expr parse_expr =
  symbol "fun" *> sep_by1 whitespace parse_pattern
  <* symbol "->"
  >>= fun params -> parse_expr >>| fun body -> LambdaExpr (params, body)
;;

let parse_product_expr parse_expr =
  let commas = symbol "," in
  let product =
    lift3
      (fun e1 e2 rest -> ProductExpr (e1, e2, rest))
      (parse_expr <* commas)
      parse_expr
      (many (commas *> parse_expr))
    <* whitespace
  in
  parenthesized product <|> product
;;

let parse_function_body parse_expr =
  many1 parse_pattern
  >>= fun patterns -> symbol "=" *> parse_expr >>| fun body -> LambdaExpr (patterns, body)
;;

let parse_let_expr parse_expr =
  symbol "let"
  *> lift4
       (fun rec_flag value_bindings and_bindings body ->
         LetExpr (rec_flag, value_bindings, and_bindings, body))
       (symbol "rec" *> (take_while1 Char.is_whitespace *> return true) <|> return false)
       (lift2
          (fun pat expr -> pat, expr)
          parse_pattern
          (symbol "=" *> parse_expr <|> parse_function_body parse_expr))
       (many
          (symbol "and"
           *> lift2
                (fun pat expr -> pat, expr)
                parse_pattern
                (symbol "=" *> parse_expr <|> parse_function_body parse_expr)))
       (symbol "in" *> parse_expr)
;;

let parse_expr =
  fix (fun expr ->
    let term =
      choice
        [ parse_name_expr
        ; parse_literal_expr
        ; parse_list_expr expr
        ; parenthesized expr
        ; parenthesized (parse_annotated_expr expr)
        ]
    in
    let func = parse_application_expr term in
    let cons = parse_optional_expr func <|> func in
    let ife = parse_conditional_expr expr <|> cons in
    let unops = parse_unary_op_expr ife <|> ife in
    let ops1 = parse_left_associative unops (multiply <|> divide) in
    let ops2 = parse_left_associative ops1 (add <|> subtract) in
    let cmp = parse_left_associative ops2 compare_ops in
    let boolean = parse_left_associative cmp (and_op <|> or_op) in
    let tuple = parse_product_expr boolean <|> boolean in
    let lambda = parse_lambda_expr expr <|> tuple in
    choice [ parse_let_expr expr; parse_lambda_expr expr; lambda ])
;;

let parse_declaration =
  let parse_eval = parse_expr >>| fun e -> ExprDeclaration e in
  let parse_binding =
    symbol "let"
    *> lift3
         (fun r id id_list -> BindingDeclaration (r, id, id_list))
         (symbol "rec" *> (take_while1 Char.is_whitespace *> return true) <|> return false)
         (lift2
            (fun pat expr -> pat, expr)
            parse_pattern
            (symbol "=" *> parse_expr <|> parse_function_body parse_expr))
         (many
            (symbol "and"
             *> lift2
                  (fun pat expr -> pat, expr)
                  parse_pattern
                  (symbol "=" *> parse_expr <|> parse_function_body parse_expr)))
  in
  choice [ parse_eval; parse_binding ]
;;

let parse_script =
  let definitions_or_exprs =
    many parse_declaration <* option () (symbol ";;" >>| ignore)
  in
  definitions_or_exprs <* whitespace
;;

let parse input = parse_string ~consume:All parse_script input
