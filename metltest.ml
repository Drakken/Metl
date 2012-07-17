
open MetlStringAux
	
let rejects rul str = rul (make_buffer str) =  None
let accepts rul str = rul (make_buffer str) <> None
    
let returns rul str x =
  match rul (make_buffer str) with
  | None -> false
  | Some (x',_) -> x' = x

(* RULES *)

let got_eoi () = accepts (rule [EOI]) ""
let not_eoi () = rejects (rule [EOI]) "a"

let right_char () = returns  (rule ['c'])   "c"   'c'    (* char parser returns char *)
let wrong_char () = rejects  (rule ['c'])   "d"

let any_char () = returns (rule [ANY]) "c" 'c'
    
let foo = rule ["foo"]

let right_token () = accepts foo "foo"
let wrong_token () = rejects foo "bar"

let either () = List.for_all (accepts (rule [('a'|'x');('1'|'9')])) ["a1";"a9";"x1";"x9"]

let bar = rule ["bar"]
let foobar = rule [foo;bar]
let binds = rule [foo:foo; bar -> foo]

let seq_last () = returns (rule ['a'; ANY]) "ab" 'b'  (* sequence val defaults to last item *)

let test5 () = accepts foobar "foobar"

let (>::) str f = (str,f)

let tests =
 ["got_eoi" >:: got_eoi
 ;"not_eoi" >:: not_eoi
 ;"right_char" >:: right_char
 ;"wrong_char" >:: wrong_char
 ;  "any_char" >::   any_char
 ;"seq_last" >:: seq_last
 ;"right_token" >:: right_token
 ;"wrong_token" >:: wrong_token
 ;"either"      >:: either
]
let run_test width (str,f) = 
  let ok = f () in
  Printf.printf "%-*s:  %s\n" width str (if ok then "pass" else "FAIL");
  ok

let longest strs = List.fold_left max 0 (List.map String.length strs)

let run_tests tests =
  let width = longest (List.map fst tests)
  in let rec iter = function
    | [] -> print_endline "Tests passed."
    | t::ts ->
	if run_test width t then iter ts
	else print_endline "ERROR:  TEST FAILED."
  in iter tests

let () = run_tests tests
