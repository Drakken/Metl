
open Utils;

open Camlp4;

module Entry = PreCast.Gram.Entry;
module OCaml = PreCast.Syntax;
module  Ast  = PreCast.Ast;
module Gram  = PreCast.Gram;

module Make (Reader: READER.T) = struct

  module AST = AST.Make(Reader);
  open AST;

  value rule   = Entry.mk "rule";
  value action = Entry.mk "action";
  value parsr  = Entry.mk "parsr";
  value sep    = Entry.mk "sep";
  value bindpat = Entry.mk "bindpat"
  ;
  value pe (p,eo) =
    let x = gensym "final_var" in
    match eo with 
    [ None -> (Binding (<:patt< $lid:x$ >>, p), <:expr< Some ($lid:x$, metlbuf) >>)
    | Some e ->
      let e' = <:expr< let $lid:x$ = $e$ in Some ($lid:x$, metlbuf) >>
      in (p,e') ]
  ;
  value action_asts peos = List.map pe peos
(*    let has_action (_,eo) = eo <> None in
    if List.for_all has_action peos then List.map pe peos
    else if not (List.exists has_action peos)
    then List.map (fun (p,_) -> (p, <:expr< Some ((), metlbuf) >> )) peos
    else failwith "Found mixed alternatives"
*)  ;
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
    parsr:
      [ "alt" [ ps = LIST1 NEXT SEP "|"-> Alts ps ]
      |  "seq"  [ p1 = SELF; ";"; p2 = NEXT -> Seq (p1,p2) ]
      | "binding" [ p = NEXT; xo = OPT bindpat -> match xo with [None -> p | Some x -> Binding (x,p)] ]
      | "unary" NONA
        [ "!"; p = NEXT -> Absent  p
        | "&"; p = NEXT -> Present p
        | "?"; e = OCaml.expr ->  Test  e
        | "OPT"; p = NEXT -> Opt p
        | "LIST0"; p = NEXT; so = OPT sep -> List0 (p,so)
        | "LIST1"; p = NEXT; so = OPT sep -> List1 (p,so) ]
      | "app" [ p = OCaml.a_LIDENT; args = LIST0 OCaml.expr -> App (p,args) ]
      | "closed"
         [ "EOI" -> Eoi
         | "ANY" -> Any
	 | "("; p = SELF; ")" -> p
         | pr = Reader.parsr -> Reader pr ] ]
  ;
  END;
end;
  
