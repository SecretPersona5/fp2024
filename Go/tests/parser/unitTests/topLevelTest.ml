(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Parse
open Pprinter.Printer
open Pp

let%expect_test "file with one var decl with ws" =
  pp print_file parse_file {|

    /* hello */  var a int
// hey

|};
  [%expect {|var a int |}]
;;

let%expect_test "file with multiple var decls separated by semicolon" =
  pp print_file parse_file {|var a, b int;var c = "hello"|};
  [%expect {|
    var a, b int

    var c = "hello" |}]
;;

let%expect_test "file with one simple func decl" =
  pp print_file parse_file {|func _() {}|};
  [%expect {|
    func _() {} |}]
;;

let%expect_test "file with one default func decl" =
  pp print_file parse_file {|func sum3(a, b, c int) int {
        return a + b + c
  }|};
  [%expect {|
    func sum3(a int, b int, c int) int {
        return a + b + c
    } |}]
;;

let%expect_test "file with mixed func and var decls" =
  pp
    print_file
    parse_file
    {|

var a = 5

func test (
// hey
) (

/* hello */	) { 
	return 
}

func id(a int) (int) {
	return a
}

var f int

func main() {
	defer test()

go println(id(10))
}

|};
  [%expect
    {|
    var a = 5

    func test() {
        return
    }

    func id(a int) int {
        return a
    }

    var f int

    func main() {
        defer test()
        go println(id(10))
    } |}]
;;

let%expect_test "file with factorial func" =
  pp
    print_file
    parse_file
    {|
  func fac(n int) int {
      if n == 1 {
          return 1  
      } else {
          return n * fac(n - 1)
      }
  }|};
  [%expect
    {|
    func fac(n int) int {
        if n == 1 {
            return 1
        } else {
            return n * fac(n - 1)
        }
    } |}]
;;

let%expect_test "tmp" =
  pp
    print_file
    parse_file
    {|
    var a, b, c chan [5]int = get()

    var x int

    func main(a2 int) bool {
        var x int
    }

    func main1(a1 int, c int, b int) bool {} |};
  [%expect
    {|
    var a, b, c chan [5]int = get()

    var x int

    func main(a2 int) bool {
        var x int
    }

    func main1(a1 int, c int, b int) bool {} |}]
;;
