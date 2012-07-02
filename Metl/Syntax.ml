(* 
 * Copyright 2012 Daniel S. Bensen
 * See LICENSE for details.
 *)

open Utils;

open Camlp4;

module Entry = PreCast.Gram.Entry;
module OCaml = PreCast.Syntax;
module  Ast  = PreCast.Ast;
module Gram  = PreCast.Gram;

module Make (Reader: READER.T) = struct

  module AST = AST.Make(Reader);
  open AST;

  ENTRIES rule action list_arg parsr sep bindpat END

  value _loc = PreCast.Loc.mk "Metl/Syntax";
  
  value default_pe x = fun
    [ Seq (p1,p2) ->
      (Seq (p1, Binding (<:patt< $lid:x$ >>, p2)), <:expr< Some ($lid:x$, metlbuf) >>)
    | p ->     (Binding (<:patt< $lid:x$ >>, p),   <:expr< Some ($lid:x$, metlbuf) >>)
  ];
  value pe (p,eo) =
    let x = gensym "final_var" in
    match eo with 
    [ None -> default_pe x p
    | Some e ->
      let e' = <:expr< let $lid:x$ = $e$ in Some ($lid:x$, metlbuf) >>
      in (p,e') ]
  ;
  value action_asts peos = List.map pe peos
  ;
  EXTEND Gram
  
    OCaml.expr: AFTER "top" [[ "rule"; "["; OPT "|"; pes = rule; "]" -> AST.rule_expr pes ]]
    ;
    rule:
      [[ peos = LIST1 [p = parsr LEVEL "seq"; eo = OPT action -> (p,eo)] SEP "|" -> action_asts peos ]]
    ;
    action: [[ "->"; e = OCaml.expr -> e]]
    ;
    sep: [[ "SEP"; p = parsr LEVEL "app" -> p ]]
    ;
    bindpat: [[ ":"; x = OCaml.ipatt -> x ]]
    ;
    list_arg: [[ p = OCaml.a_LIDENT -> App (p,[]) | p = parsr LEVEL "closed" -> p ]]
    ;
    parsr:
      [ "alt" [ ps = LIST1 NEXT SEP "|"-> Alts ps ]
      |  "seq"  [ p1 = SELF; ";"; p2 = NEXT -> Seq (p1,p2) ]
      | "binding" [ p = NEXT; xo = OPT bindpat -> match xo with [None -> p | Some x -> Binding (x,p)] ]
      | "app"
	[ p = OCaml.a_LIDENT; args = LIST0 (OCaml.expr LEVEL "simple") -> App (p,args) ]
      | "unary" NONA
        [ "!"; p = NEXT -> Absent  p
        | "&"; p = NEXT -> Present p
        | "?"; e = OCaml.expr ->  Test  e
        | "OPT"; p = NEXT -> Opt p
        | "LIST0"; p = list_arg; so = OPT sep -> List0 (p,so)
        | "LIST1"; p = list_arg; so = OPT sep -> List1 (p,so) ]
      | "closed"
         [ "EOI" -> Eoi
         | "ANY" -> Any
	 | "("; p = SELF; ")" -> p
         | pr = Reader.parsr -> Reader pr ] ]
  ;
  END;
end;
  
