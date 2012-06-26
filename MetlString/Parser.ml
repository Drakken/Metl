
module Id = struct
  value name = "MetlString";
  value version = "0.0.1";
end;

open Camlp4.PreCast;

module Make (U: sig end) = struct

  module Reader : Metl.READER.T = struct
  
    open Metl.Utils
    ;
    value next =
    <:expr< MetlStringAux.Aux.((metlbuf.string.[metlbuf.n],
				{(metlbuf) with n = metlbuf.n + 1})) >>
    ;
    value eoi = <:expr< MetlStringAux.Aux.(metlbuf.n = metlbuf.length) >>
    ;
    value next_char_test_name c = 
      if      Base. is_word_char  c then  "is_word_char"
      else if Base.is_symbol_char c then "is_symbol_char"
      else if Base.is_delim_char  c then   "is_no_char"
      else failwith ("unclassified char: '" ^ Char.escaped c ^ "'")
    ;
    value token_expr tok =
      let f = next_char_test_name tok.[String.length tok - 1] in
      <:expr< MetlStringAux.Aux.has_token MetlStringAux.Base.$lid:f$ $str:tok$ metlbuf >>
    ;
    type ast =
      [ String of (string*string)
      | Char  of  (char*string)
      | Range of ((char*string)*(char*string)) ]
    ;
    value rec parser_expr x p enone esome =
      match p with
      [ String s _ ->
	let len = String.length s in
	<:expr<
	if not $token_expr s$ then $enone$
	else
	  let metlbuf = MetlStringAux.Aux.({(metlbuf) with n = metlbuf.n + $int: string_of_int len$})
	  in $esome$ >>
      | Char (_,str) ->
          <:expr< match (MetlStringAux.Aux.char $chr:str$ metlbuf) with
                  [ None -> $enone$
                  | Some ($pat:x$, metlbuf) -> $esome$ ] >>
      | Range ((_,str1),(_,str2)) ->
          <:expr< match (MetlStringAux.Aux.range $chr:str1$ $chr:str2$ metlbuf) with
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
        [ `STRING s s' -> String (s,s')
        | `CHAR  c1 s1; rest = OPT range ->
	    match rest with 
	      [ None -> Char (c1,s1)
	      | Some (c2,s2) -> Range ((c1,s1),(c2,s2))]]]
    ;END;
    
  end;
    
  module M = Metl.Syntax.Make(Reader);
    
end;

module M = Camlp4.Register.Plugin (Id) (Make);
