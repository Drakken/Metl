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

  module Pst = Pst.Make(Reader);
  open Pst;

  type entry 'a = Gram.Entry.t 'a;

  value alternative: entry (pst * Ast.expr)
                                   = Gram.Entry.mk "alternative"; Gram.Entry.clear alternative;
  value     rule                   = Gram.Entry.mk     "rule"   ; Gram.Entry.clear rule;
  value    action  : entry Ast.expr= Gram.Entry.mk    "action"  ; Gram.Entry.clear action;
  value   list_arg : entry pst     = Gram.Entry.mk   "list_arg" ; Gram.Entry.clear list_arg;
  value     sep    : entry pst     = Gram.Entry.mk     "sep"    ; Gram.Entry.clear sep;
  value   bindpat  : entry Ast.patt= Gram.Entry.mk   "bindpat"  ; Gram.Entry.clear bindpat;
  value  inner_alts: entry pst     = Gram.Entry.mk  "inner_alts"; Gram.Entry.clear inner_alts;
  value   sequence : entry pst     = Gram.Entry.mk   "sequence" ; Gram.Entry.clear sequence;
  value   seq_item : entry pst     = Gram.Entry.mk   "seq_item" ; Gram.Entry.clear seq_item;
  value   binding  : entry pst     = Gram.Entry.mk   "binding"  ; Gram.Entry.clear binding;
  
  value ensure_action _loc pst = fun
    [ Some e -> (pst, <:expr< Some ($e$,metlbuf) >>)
    | None ->
	let x = gensym "final_val"
	in (Binding (_loc, <:patt< $lid:x$ >>, pst), <:expr< Some ($lid:x$, metlbuf) >>)
    ]
  ;
  EXTEND Gram
    OCaml.expr: AFTER "top" [["rule"; "["; OPT "|"; pes = rule; "]" -> rule_expr _loc pes]]
    ;
    rule: [[pes = LIST1 alternative SEP "|" -> pes]]
    ;
    alternative: [[pst = sequence; eo = OPT action -> ensure_action _loc pst eo]]
    ;
    action: [["->"; e = OCaml.expr -> e]]
    ;
    sep: [["SEP"; p = binding LEVEL "app" -> p]]
    ;
    bindpat: [[":"; pat = OCaml.ipatt -> pat]]
    ;
    list_arg: [[p = OCaml.a_LIDENT -> App (_loc,p,[]) | p = binding LEVEL "closed" -> p]]
    ;
    inner_alts: [[psts = LIST1 sequence SEP "|" -> Alts (_loc,psts)]]
    ;
    seq_item: [[";"; lpst = binding -> lpst]]
    ;
    sequence: [[
      pst = binding; psts = LIST0 seq_item
        -> if psts = [] then pst else Seq (_loc, [pst::psts])
    ]];
    binding: [[pst = NEXT; lpo = OPT bindpat ->
           match lpo with
           [ None -> pst
           | Some pat -> Binding (_loc, pat, pst)]]
      | "app"
	[p = OCaml.a_LIDENT; args = LIST0 (OCaml.expr LEVEL "simple") -> App (_loc,p,args)]
      | "unary" NONA
        [ "!"; p = NEXT -> Absent  (_loc,p)
        | "&"; p = NEXT -> Present (_loc,p)
        | "?"; e = OCaml.expr ->  Test  (_loc,e)
        | "OPT"; p = NEXT -> Opt (_loc,p)
        | "LIST0"; p = list_arg; so = OPT sep -> List0 (_loc,p,so)
        | "LIST1"; p = list_arg; so = OPT sep -> List1 (_loc,p,so) ]
      | "closed"
         [ "EOI" -> Eoi _loc
         | "ANY" -> Any _loc
	 | "("; p = inner_alts; ")" -> p
         | pr = Reader.parsr -> Reader (_loc,pr) ] ]
  ;
  END;
end;
  
