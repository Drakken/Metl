(* 
 * Copyright 2012 Daniel S. Bensen
 * See LICENSE for details.
 *)

module Id = struct
  value name = "MetlString";
  value version = "0.0.6";
end;

open Camlp4.PreCast;

module Make (U: sig end) = struct

  module Reader : Metl.READER.T = struct
  
    open Metl.Utils
    ;
    value next _loc =
    <:expr< MetlStringAux.((metlbuf.str.[metlbuf.n],
				{(metlbuf) with n = metlbuf.n + 1})) >>
    ;
    value eoi _loc = <:expr< MetlStringAux.(metlbuf.n = metlbuf.length) >>
    ;
    value next_char_test_name c = 
      if      MetlStringBase. is_word_char  c then  "is_word_char"
      else if MetlStringBase.is_symbol_char c then "is_symbol_char"
      else if MetlStringBase.is_delim_char  c then   "is_no_char"
      else failwith ("unclassified char: '" ^ Char.escaped c ^ "'")
    ;
    value token_expr _loc tok =
      let f = next_char_test_name tok.[String.length tok - 1] in
      <:expr< MetlStringAux.has_token MetlStringAux.$lid:f$ $str:tok$ metlbuf >>
    ;
    type pst =
      [ String of (Loc.t * string * string)
      | Char  of  (Loc.t * char * string)
      | Range of  (Loc.t * (char*string) * (char*string)) ]
    ;
    value rec parser_expr x pst enone esome =
      match pst with
      [ String _loc s _ ->
	let len = String.length s in
	<:expr<
	if not $token_expr _loc s$ then $enone$
	else
	  let ($x$,metlbuf) =
            ((), MetlStringAux.({(metlbuf) with n = metlbuf.n + $int: string_of_int len$}))
	  in $esome$ >>
      | Char (_loc,_,str) ->
          <:expr< match (MetlStringAux.char $chr:str$ metlbuf) with
                  [ None -> $enone$
                  | Some ($pat:x$, metlbuf) -> $esome$ ] >>
      | Range (_loc,(_,str1),(_,str2)) ->
          <:expr< match (MetlStringAux.range $chr:str1$ $chr:str2$ metlbuf) with
                  [ None -> $enone$
                  | Some ($pat:x$, metlbuf) -> $esome$ ] >>
    ]
    ;
    value parsr  = Gram.Entry.mk "parsr";
    value range  = Gram.Entry.mk "parsr";
  
    EXTEND Gram
      range: [[".."; `CHAR c s -> (c,s)]]
      ;
      parsr: [
        [ `STRING s s' -> String (_loc,s,s')
        | `CHAR  c1 s1; rest = OPT range ->
	    match rest with 
	      [ None -> Char (_loc,c1,s1)
	      | Some (c2,s2) -> Range (_loc,(c1,s1),(c2,s2))]]]
    ;END;
    
  end;
    
  module M = Metl.Syntax.Make(Reader);
    
end;

module M = Camlp4.Register.Plugin (Id) (Make);
