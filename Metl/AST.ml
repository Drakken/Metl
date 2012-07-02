(* 
 * Copyright 2012 Daniel S. Bensen
 * See LICENSE for details.
 *)

open Utils;
open Printf;
open Camlp4.PreCast;

value _loc = Loc.mk "Metl/AST";

module Make (Reader: READER.T) = struct
  type ast =
    [ Reader of Reader.ast
    | App of (string * list Ast.expr)
    | Seq of (ast * ast)
    | Binding of (Ast.patt * ast)
    | Absent  of  ast
    | Present of  ast
    | List0   of (ast * option ast)
    | List1   of (ast * option ast)
    | Opt     of  ast
    | Test    of  Ast.expr
    | Alts  of list ast
    | Any
    | Eoi ]
  ;
  value bind pat fname args enone esome =
    let fx f x = <:expr< $f$ $x$ >> in
    let ef = List.fold_left fx <:expr< $lid:fname$ >> args in
     <:expr< match $ef$ metlbuf with
        [ None -> $enone$ | Some ($pat$,metlbuf)-> $esome$ ] >>
  ;
  value __ = <:patt< _ >>
  ;
  value unit_expr x e = <:expr< let $x$ = () in $e$ >>
  ;
  value rec
      parser_expr pat p enone esome = match p with
    [ Reader pr -> Reader.parser_expr pat pr enone esome
    | App    fname args -> bind pat fname args enone esome
    | Seq   (p1,p2) -> seq pat p1 p2 enone esome
    | Binding(pat',p')-> do { assert (pat=__); parser_expr pat' p' enone esome }
    | Absent  p    -> absent  p    enone (unit_expr pat esome)
    | Present p    -> present p    enone (unit_expr pat esome)
    | List0  (p',pso) -> list0 pat  p' pso         esome
    | List1  (p',pso) -> list1 pat  p' pso   enone esome
    | Opt     p       ->  opt  pat  p              esome
    | Test    e    -> do { assert (pat=__); affirm   e   enone esome }
    | Alts ps -> alts pat ps enone esome
    | Any -> any pat enone esome
    | Eoi -> eoi enone esome
    ]
  and
      seq pat p1 p2 enone esome =
    parser_expr __ p1 none (parser_expr pat p2 enone esome)
  and
      test_parser p tfnone tfsome enone esome =
    let rzult  = gensym "rzult"  in
    let ep = parser_expr __ p <:expr< $uid:tfnone$ >> <:expr< $uid:tfsome$ >>
    in <:expr<
    let $lid:rzult$ = $ep$
    in if $lid:rzult$ then $enone$ else $esome$ >>
  and
      absent  p enone esome = test_parser p "False" "True" enone esome
  and present p enone esome = test_parser p "True" "False" enone esome
  and
      opt pat p e =
    let oldbuf = gensym "oldbuf" in
    let xtemp  = gensym "xtemp" in
    let xtpat = <:patt< $lid:xtemp$ >> in
    let ep = parser_expr xtpat p <:expr< Some (    None    , $lid:oldbuf$) >>
                                 <:expr< Some (Some $lid:xtemp$, metlbuf ) >>
    in <:expr<
         let $lid:oldbuf$ = metlbuf in
         let ($pat:pat$,metlbuf) = ($ep$) in $e$ >>
  and
      list0 xs_final p pso esome =
    let    x  = gensym "x"  (* one item *)
    in let xpat = <:patt< $lid:x$ >>
    in let xs = gensym "xs"  (* the tail *)
    in let f  = gensym "f"   (* the entry point *)
    in let f0 = gensym "f0"  (* parses one item,  no  separator  *)
    in let f1 = gensym "f1"  (* parses one item, separator first *)
    in let fp = gensym "fp"  (* local name for f0 or f1 *)
    in let ep0 = parser_expr xpat p none <:expr< Some ($lid:x$, metlbuf) >>
    in let ep1 =
      match pso with [ None ->                <:expr< $lid:f0$ metlbuf >>
      | Some psep -> parser_expr __ psep none <:expr< $lid:f0$ metlbuf >> ]
    in <:expr<
      let    $lid:f0$ metlbuf = $ep0$
      in let $lid:f1$ metlbuf = $ep1$
      in let rec $lid:f$ $lid:fp$ metlbuf $lid:xs$ =
        match $lid:fp$ metlbuf with
        [ None -> (List.rev $lid:xs$, metlbuf)
        | Some ($lid:x$, metlbuf) ->
            $lid:f$ $lid:f1$ metlbuf [$lid:x$ :: $lid:xs$] ]
      in
      let ($pat:xs_final$, metlbuf) = $lid:f$ $lid:f0$ metlbuf []
      in $esome$ >>
  and
      list1 xs p pso enone esome =
    let xs0 = gensym "xs0"
    in let pat0 = <:patt< $lid:xs0$ >>
    in let e0 = list0 pat0 p pso <:expr< ($lid:xs0$,metlbuf) >>
    in <:expr< match ($e0$) with
         [ (   []   ,    _   ) -> $enone$
         | ($pat:xs$, metlbuf) -> $esome$ ] >>
  and
     affirm e enone esome = <:expr< if $e$ then $esome$ else $enone$ >>
  and
      alts_expr pat pes enone = match pes with
    [ [] -> assert False
    | [pe1::pes] ->
      let buf0 = gensym "buf0"
      in let alt (p,esome) enone = parser_expr pat p enone esome
      in let combine pe enone =
        <:expr< let metlbuf = $lid:buf0$ in $alt pe enone$ >>
      in let enone1 = List.fold_right combine pes enone
      in <:expr< let $lid:buf0$ = metlbuf in $alt pe1 enone1$ >> ]
  and
      alts pat ps enone esome = alts_expr pat (List.map (fun p -> (p,esome)) ps) enone
      (* FIXME: esome needs to be wrapped in a function *)
  and
      any pat enone esome =
    let esome' = <:expr< let ($pat$,metlbuf) = $Reader.next$ in $esome$ >>
    in parser_expr __ (Absent Eoi) enone esome'
  and
      eoi enone esome = <:expr< if $Reader.eoi$ then $esome$ else $enone$ >>
  
  ;value func e = <:expr< fun metlbuf -> $e$ >>
  
  ;value rule_expr = fun
    [ [] -> failwith "empty rule body"
    | [(p,esome)] -> func (parser_expr __ p none esome)
    |   p_esomes   -> func (alts_expr __ p_esomes none)
    ];
    
end;
