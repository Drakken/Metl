
open Metl
open Core

open OUnit;;

let test1 () = assert_equal "x" (Foo.unity "x");; 

let test2 () = assert_equal 100 (Foo.unity 100);;

(* Name the test cases and group them together *)
let suite = 
"suite">:::
 ["test1">:: test1;
  "test2">:: test2]
;;

let _ = 
  run_test_tt_main suite
;;


(*

let code = "foo = bar\n"

let defn = seqs [lid;ows;symchar;ows;lid]

let () =
  match defn (code,0) with
  | None -> print_endline "***FAIL***"
  | Some (_,n) -> print_int n; print_newline()

*)
