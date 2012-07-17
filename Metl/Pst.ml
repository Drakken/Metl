(* 
 * Copyright 2012 Daniel S. Bensen
 * See LICENSE for details.
 *)

open Utils;
open Printf;
open Camlp4.PreCast;

module Make (Reader: READER.T) = struct
  type pst =
    [ Reader of  (Loc.t * Reader.pst)
    | App     of (Loc.t * string * list Ast.expr)
    | Binding of (Loc.t * Ast.patt * pst)
    | Absent  of (Loc.t * pst)
    | Present of (Loc.t * pst)
    | List0   of (Loc.t * pst * option pst)
    | List1   of (Loc.t * pst * option pst)
    | Opt     of (Loc.t * pst)
    | Test    of (Loc.t * Ast.expr)
    | Seq     of (Loc.t * list pst)
    | Alts    of (Loc.t * list pst)
    | Any     of  Loc.t 
    | Eoi     of  Loc.t ]
  ;
  value bind _loc pat fname args enone esome =
    let fx f x = <:expr< $f$ $x$ >> in
    let ef = List.fold_left fx <:expr< $lid:fname$ >> args in
     <:expr< match $ef$ metlbuf with
        [ None -> $enone$ | Some ($pat$,metlbuf)-> $esome$ ] >>
  ;
  value __ _loc = <:patt< _ >>
  ;
  value assert__ = fun [Ast.PaAny _ -> () | _ -> failwith "Bad pattern"]
  ;
  value lid_of_pat = fun
  [ <:patt< $lid:x$ >> -> x
  | <:patt< _ >> -> gensym "lid_of_pat"
  | _ -> failwith "invalid pattern: must be an LIDENT" ]
  ;
  value rec
      parser_expr pat pst enone esome = match pst with
    [ Reader (_loc,pr) -> Reader.parser_expr pat pr enone esome
    | App    (_loc,fname,args) -> bind _loc pat fname args enone esome
    | Alts   (_loc,psts) -> inner_alts _loc (lid_of_pat pat) psts enone esome
    | Seq    (_loc,psts) ->    sequence     _loc pat psts enone esome
    | Binding(_loc,pat',pst')-> do { assert__ pat; parser_expr pat' pst' enone esome }
    | Absent (_loc,pst)    -> absent  _loc pat pst    enone esome
    | Present(_loc,pst)    -> present _loc pat pst    enone esome
    | List0  (_loc,pst',pso) -> list0 _loc pat  pst' pso         esome
    | List1  (_loc,pst',pso) -> list1 _loc pat  pst' pso   enone esome
    | Opt    (_loc,pst)       ->  opt _loc  pat  pst              esome
    | Test   (_loc,e)    -> do { assert__ pat; affirm  _loc  e   enone esome }
    | Any     _loc -> any _loc pat enone esome
    | Eoi     _loc -> eoi _loc pat enone esome
    ]
  and
      sequence _loc pat psts enone esome =
    match List.rev psts with 
    [ [] -> failwith "empty sequence"
    | [pst::psts] ->
    let pst = Binding (_loc,pat,pst)      (* The value of a sequence is  *)
    in let psts = List.rev [pst::psts]    (* the value of its last item. *)
    in let  fnone = gensym "fnone"
    in let efnone = <:expr< $lid:fnone$ () >>
    in let combine pst esome = parser_expr (__ _loc) pst efnone esome
    in let e = List.fold_right combine psts esome
    in <:expr< let $lid:fnone$ () = $enone$ in $e$ >> ]
  and
      absent _loc pat pst enone esome =
    let etest = parser_expr (__ _loc) pst <:expr<False>> <:expr<True>>
    in <:expr< if $etest$ then $enone$ else let $pat$ = () in $esome$ >>
  and
      present _loc pat p enone esome =
    let buf0  = gensym "buf0"
    in let esome' = <:expr< let metlbuf = $lid:buf0$ in $esome$ >>
    in let e = parser_expr pat p enone esome'
    in <:expr< let $lid:buf0$ = metlbuf in $e$ >>
  and
      opt _loc pat p e =
    let oldbuf = gensym "oldbuf" in
    let xtemp  = gensym "xtemp" in
    let xtpat = <:patt< $lid:xtemp$ >> in
    let ep = parser_expr xtpat p <:expr< Some (    None    , $lid:oldbuf$) >>
                                 <:expr< Some (Some $lid:xtemp$, metlbuf ) >>
    in <:expr<
         let $lid:oldbuf$ = metlbuf in
         let ($pat:pat$,metlbuf) = ($ep$) in $e$ >>
  and
      list0 _loc pat pst pstsepo esome =
    let    x  = gensym "x"  (* one item *)
    in let xpat = <:patt< $lid:x$ >>
    in let xs = gensym "xs"  (* the tail *)
    in let f  = gensym "f"   (* the entry point *)
    in let f0 = gensym "f0"  (* parses one item,  no  separator  *)
    in let f1 = gensym "f1"  (* parses one item, separator first *)
    in let fp = gensym "fp"  (* local name for f0 or f1 *)
    in let ep0 = parser_expr xpat pst (none _loc) <:expr< Some ($lid:x$, metlbuf) >>
    in let ep1 =
      match pstsepo with [ None ->                <:expr< $lid:f0$ metlbuf >>
      | Some pstsep -> parser_expr (__ _loc) pstsep (none _loc) <:expr< $lid:f0$ metlbuf >> ]
    in <:expr<
      let    $lid:f0$ metlbuf = $ep0$
      in let $lid:f1$ metlbuf = $ep1$
      in let rec $lid:f$ $lid:fp$ metlbuf $lid:xs$ =
        match $lid:fp$ metlbuf with
        [ None -> (List.rev $lid:xs$, metlbuf)
        | Some ($lid:x$, metlbuf) ->
            $lid:f$ $lid:f1$ metlbuf [$lid:x$ :: $lid:xs$] ]
      in
      let ($pat$, metlbuf) = $lid:f$ $lid:f0$ metlbuf []
      in $esome$ >>
  and
      list1 _loc xs p pso enone esome =
    let xs0 = gensym "xs0"
    in let pat0 = <:patt< $lid:xs0$ >>
    in let e0 = list0 _loc pat0 p pso <:expr< ($lid:xs0$,metlbuf) >>
    in <:expr< match ($e0$) with
         [ (   []   ,    _   ) -> $enone$
         | ($pat:xs$, metlbuf) -> $esome$ ] >>
  and
     affirm _loc e enone esome = <:expr< if $e$ then $esome$ else $enone$ >>
  and
      alts_expr _loc pat pes enone = match pes with
    [ [] -> assert False
    | [pe1::pes] ->
    let buf0 = gensym "buf0"
    in let alt (p,esome) enone = parser_expr pat p enone esome
    in let combine pe enone =
      <:expr< let metlbuf = $lid:buf0$ in $alt pe enone$ >>
    in let enone1 = List.fold_right combine pes enone
    in <:expr< let $lid:buf0$ = metlbuf in $alt pe1 enone1$ >> ]
  and
      inner_alts _loc x ps enone esome =
    let     fsome = gensym "fsome"
    in let efsome = <:expr< $lid:fsome$ $lid:x$ metlbuf >>
    in let e = alts_expr _loc <:patt< $lid:x$ >> (List.map (fun p -> (p,efsome)) ps) enone
    in <:expr< let $lid:fsome$ $lid:x$ metlbuf = $esome$ in $e$ >>
  and
      any _loc pat enone esome =
    let esome' = <:expr< let ($pat$,metlbuf) = $Reader.next _loc$ in $esome$ >>
    in parser_expr (__ _loc) (Absent (_loc, (Eoi _loc))) enone esome'
  and
      eoi _loc pat enone esome =
        <:expr< let $pat$ = () in if $Reader.eoi _loc$ then $esome$ else $enone$ >>
 
  ;value func _loc e = <:expr< fun metlbuf -> $e$ >>
  
  ;value rule_expr _loc = fun
    [ [] -> failwith "empty rule body"
    | [(p,esome)] -> func _loc (parser_expr (__ _loc) p (none _loc) esome)
    |   p_esomes   -> func _loc (alts_expr _loc (__ _loc) p_esomes (none _loc))
    ];
    
end;
