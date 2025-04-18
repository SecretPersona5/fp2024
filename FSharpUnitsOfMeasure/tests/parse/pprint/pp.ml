(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Base

let pp printer parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> Stdlib.print_endline (printer res)
  | Error res -> Stdlib.print_endline res
;;

let pp2 printer parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer Stdlib.Format.std_formatter res
  | Error res -> Stdlib.print_endline res
;;
