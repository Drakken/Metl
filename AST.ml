
open Printf

;value require test str = if not test then failwith str

;value undefined () = failwith "This function has not been implemented."

;value name_generator prefix ?(suffix="") ?(n1=1) () =
  let n = ref (n1-1) in
  fun ?(suffix=suffix) () -> n := n.val + 1; prefix ^ string_of_int n.val ^ suffix

;value genloc = name_generator "metl_loc_" ()

;type t =
  | Alt     of b*b
  | Action  of b*e
  | Seq     of b*b
  | Binding of b*string
  | Deny    of b
  | Affirm  of b
  | Test    of expr
  | Many0   of b
  | Many1   of b
  | Opt     of b 
  | Lid     of string

;value rec
    parser_expr x none_opt some = fun
  [ Lid     str   -> lid x none_opt some str
  | Seq     p1 p2 -> seq x none_opt some p1 p2
  | Binding p str -> binding x none_opt some p str
  | Deny    p -> undefined ()
  | Affirm  p -> undefined ()
  | Many0   p -> undefined ()
  | Many1   p -> undefined ()
  | Opt     p -> undefined ()
  | Test    e -> undefined ()
  | Alt     p1 p2 -> alt x e p1 p2


 ]

and lid_expr x none_opt some str =
  let e =
    match none_opt with
    [ None      -> <:expr< match $lid:str$ _loc with [ None -> None  |  Some ($x$,_loc) -> $some$ ] >>
    | Some none -> <:expr< match $lid:str$ _loc with [ Some ($x$,_loc) -> $some$ | None -> $none$ ] >> ]
  in ("_",e)

and binding_expr x0 none_opt some p str =
  let (x1,f) = parser_expr x0 p in
  require (x1=="_") ("Found conflicting names "^x1^" and "^str^".");
  (str,f)

and seq_expr x0 none_opt some p1 p2 =
  let (x1,f1) = parser_expr x0 p1 in
  let (x2,f2) = parser_expr x1 p2 in
  let f some_expr = f1 (f2 some_expr)
  in (x2,f)

and alts_expr x0 e ps =
  let xfs = List.map (parser_expr x0) ps in
  let loc0 = genloc() in
  let f p e = <:expr< let _loc = $loc$ in $e$ >> in

  let f0 some_expr = <:expr<
    match $f1 some1$ with
    | Some (x,_loc) ->
    | None -> $f2 some2$ >>
  in let f = 
    <:expr< let $loc0$ = _loc in ...
  in (x2,f)

;value failure = <:expr< None >>
;value wrap e = <:expr< Some ($e$, _loc) >>

;value rec body_expr body =
  match body with
  [ [] -> failwith "empty rule body"
  | [(p,e)] -> wrap (parser_expr "_" failure e p)
  | (p,ep)::pes ->
    let loc = genloc() in
    let combine (p,e_some) e_none =
      let expr = parser_expr "_" (Some e_none) e_some p in
        <:expr< let _loc = $loc$ in $expr$ >> in
    let tail = List.fold_right combine pes failure in
    let expr = parser_expr "_" (Some tail) ep p in
    wrap <:expr< let $loc$ = _loc in $expr$ >>


value add_arg x e = <:expr< fun $x$ -> $e$ >>

value rule_expr f args body =
 let body =
    match body with
    [ Action x | Alt x -> body
    | _ -> Action (body, <:expr< () >>) ]

 in
    let (_,body_func) = start_func "_" _loc body
    in 
    body_func _loc_out
      <:expr< None >>
      <expr:< Some ($e$, $_loc_out$) >>
  in let args_expr = List.fold_right add_arg args e
  in <:expr let $f$ = $args_expr$ >>


(*
  let (body,e) =
  in
*)
