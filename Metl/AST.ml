
open Utils;
open Printf;
open Camlp4.PreCast;

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
  value bind x p args enone esome =
    let fx f x = <:expr< $f$ $x$ >> in
    let f = List.fold_left fx <:expr< $lid:p$ >> args in
     <:expr< match $f$ metlbuf with
        [ None -> $enone$ | Some ($pat:x$,metlbuf)-> $esome$ ] >>
  ;
  value __ = <:patt< _ >>
  ;
(*  value patvar = fun
   [ <:patt< $lid:x$ >> -> x
   | _ -> failwith "LIST binding must be a simple name" ]
  ; *)
  value unit_expr x e = <:expr< let $x$ = () in $e$ >>
  ;
  value rec
      parser_expr x p enone esome = match p with
    [ Reader pr -> Reader.parser_expr x pr enone esome
    | App    p args -> bind x p args enone esome
    | Seq   (p1,p2) -> seq x p1 p2 enone esome
    | Binding(x',p')-> do { assert (x=__); parser_expr x' p' enone esome }
    | Absent  p    -> absent  p    enone (unit_expr x esome)
    | Present p    -> present p    enone (unit_expr x esome)
    | List0  (p',po) -> (* let x = patvar x in *) list0 x  p' po         esome
    | List1  (p',po) -> list1 x  p' po   enone esome
    | Opt     p       ->  opt  x  p              esome
    | Test    e    -> do { assert (x=__); affirm   e   enone esome }
    | Alts ps -> alts x ps enone esome
    | Any -> any x enone esome
    | Eoi -> eoi enone esome
    ]
  and
      seq x p1 p2 none some =
    if x=__ then parser_expr __ p1 none (parser_expr __ p2 none some)
    else invalid_arg "binding in seq"
  and
      test_parser p tfnone tfsome enone esome =
    let rzult  = gensym "rzult"  in
    let ep = parser_expr __ p <:expr< $uid:tfnone$ >> <:expr< $uid:tfsome$ >>
    in <:expr<
    let $lid:rzult$ = $ep$ in
    if $lid:rzult$ then $enone$ else $esome$ >>
  and
      absent  p enone esome = test_parser p "False" "True" enone esome
  and present p enone esome = test_parser p "True" "False" enone esome
  and
      opt x p e =
    let oldbuf = gensym "oldbuf" in
    let xtemp  = gensym "xtemp" in
    let xtpat = <:patt< $lid:xtemp$ >> in
    let ep = parser_expr xtpat p <:expr< (    None    , $lid:oldbuf$) >>
                                 <:expr< (Some $lid:xtemp$, metlbuf ) >>
    in <:expr<
         let $lid:oldbuf$ = metlbuf in
         let ($pat:x$,metlbuf) = ($ep$) in $e$ >>
  and
      list0 xs_final p po esome =
    let    x  = gensym "x"  (* one item *)
    in let xpat = <:patt< $lid:x$ >>
    in let  xs  = gensym "xs"  (* the tail *)
(*    in let xspat = <:patt< $lid:xs$ >> *)
    in let f  = gensym "f"  (* the entry point *)
    in let f0 = gensym "f0"  (* parses one item,  no  separator  *)
    in let f1 = gensym "f1"  (* parses one item, separator first *)
    in let fp = gensym "fp"  (* arg name for f0 or f1 *)
    in let ep0 = parser_expr xpat p none <:expr< Some ($lid:x$, metlbuf) >>
    in let ep1 =
      match po with [ None ->                  <:expr< $lid:f0$ metlbuf >>
      | Some psep -> parser_expr __ psep none <:expr< $lid:f0$ metlbuf >> ]
    in <:expr<
      let $lid:f0$ metlbuf = $ep0$ in
      let $lid:f1$ metlbuf = $ep1$ in
      let rec $lid:f$ $lid:fp$ metlbuf $lid:xs$ =
        match $lid:fp$ metlbuf with
        [ None -> (List.rev $lid:xs$, metlbuf)
        | Some ($lid:x$, metlbuf) -> $lid:f$ $lid:f1$ metlbuf [$lid:x$ :: $lid:xs$] ]
      in
      let ($pat:xs_final$, metlbuf) = $lid:f$ $lid:f0$ metlbuf [] in $esome$ >>
  and
      list1 xs p po enone esome =
    let xs0 = gensym "xs0"
    in let pat0 = <:patt< $lid:xs0$ >>
    in let e0 = list0 pat0 p po <:expr< ($lid:xs0$,metlbuf) >>
    in <:expr< match ($e0$) with
         [ (   []   ,    _   ) -> $enone$
         | ($pat:xs$, metlbuf) -> $esome$ ] >>
  and
     affirm e enone esome = <:expr< if $e$ then $esome$ else $enone$ >>
  and
      alts_expr x peos enone = match peos with
    [ [] -> assert False
    | [pe1::pes] ->
      let buf0 = genbuf "0" in
      let alt (p,e) e_none =
        let ep = parser_expr x p <:expr< None >> e in
          <:expr<
            let metlbuf = $ep$ in
            if metlbuf <> None then metlbuf
            else $e_none$ >>
      in let combine pe enone = <:expr< let metlbuf = $lid:buf0$ in $alt pe enone$ >>
      in let tail = List.fold_right combine pes enone
      in <:expr< let $lid:buf0$ = metlbuf in $alt pe1 tail$ >> ]
  and
      alts x ps enone esome = alts_expr x (List.map (fun p -> (p,esome)) ps) enone
  and
      any x enone esome =
    let esome' = <:expr< let ($x$,metlbuf) = $Reader.next$ in $esome$ >>
    in parser_expr __ (Absent Eoi) enone esome'
  and
      eoi enone esome = <:expr< if $Reader.eoi$ then $esome$ else $enone$ >>
  
  ;value func e = <:expr< fun metlbuf -> $e$ >>
  
  ;value rule_expr = fun
    [ [] -> failwith "empty rule body"
    | [(p,e)] -> func (parser_expr __ p none e)
    |   pes   -> func (alts_expr __ pes none)
    ];
    
end;
