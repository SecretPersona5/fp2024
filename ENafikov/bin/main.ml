(** Copyright 2024-2025,Ruslan Nafikov  *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ENafikov_lib
open Inferencer
open Interpret
open Parser
open Printf
open Ast

let run_type_inference input =
  match parse input with
  | Ok parsed_ast ->
    (match run_inference parsed_ast with
     | Ok type_context ->
       let filtered_context =
         Base.Map.filter_keys type_context ~f:(fun key ->
           not (List.mem key [ "print_int"; "print_endline"; "print_bool" ]))
       in
       Base.Map.iteri filtered_context ~f:(fun ~key ~data:(Forall (_, type_expr)) ->
         Format.printf "val %s: %a\n" key print_type type_expr)
     | Error inference_error -> Format.printf "Type inference error. %a\n" print_error inference_error)
  | Error parse_error -> Format.printf "Parsing error. %s\n" parse_error
;;

let execute_interpreter source_code =
  let open Stdlib.Format in
  match Parser.parse source_code with
  | Ok parsed_ast ->
    (match Eval.evaluate_script parsed_ast with
     | Ok _ -> ()
     | Error runtime_error -> printf "Runtime error: %a\n" print_runtime_error runtime_error)
  | Error parse_error -> printf "Parsing error: %s\n" parse_error
;;

let load_file_content filename =
  let input_channel = open_in filename in
  let file_content = really_input_string input_channel (in_channel_length input_channel) in
  close_in input_channel;
  file_content
;;

type runtime_config =
  { enable_type_inference : bool
  ; enable_interpreter : bool
  ; input_file : string option
  ; direct_input : string option
  }

let parse_command_line_args () =
  let rec process_args args current_config =
    match args with
    | [] -> current_config
    | "-infer" :: remaining -> process_args remaining { current_config with enable_type_inference = true }
    | "-interpret" :: remaining -> process_args remaining { current_config with enable_interpreter = true }
    | "-file" :: filename :: remaining -> process_args remaining { current_config with input_file = Some filename }
    | arg :: remaining -> process_args remaining { current_config with direct_input = Some arg }
  in
  process_args
    (Array.to_list Sys.argv |> List.tl)
    { enable_type_inference = false; enable_interpreter = false; input_file = None; direct_input = None }
;;

let program_entry () =
  let config = parse_command_line_args () in
  let source_input =
    match config.input_file with
    | Some filename -> load_file_content filename
    | None ->
      (match config.direct_input with
       | Some input_string -> input_string
       | None -> "")
  in
  if config.enable_type_inference
  then run_type_inference source_input
  else if config.enable_interpreter
  then execute_interpreter source_input
  else printf "Please specify either -infer or -interpret flag.\n"
;;

let () = program_entry ()