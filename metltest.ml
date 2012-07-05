
open Metl


let c = rule ['c']

let foo s = rule ["foo"]


let returns rul str val =
  match rul str with
  | None -> false
  | Some (x,_) -> x = val

let rejects rul str = rul str = None

open OUnit;;

let test1 () = returns  c   "c"   'c'
let test2 () = rejects  c   "d"
let test3 () = returns foo "foo" "foo"
let test4 () = rejects foo "bar"


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
