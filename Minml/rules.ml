
open Camlp4.PreCast;

value _loc = Loc.mk "rules.ml";

value fun_app =
  let apply1 ef x = <:expr< $ef$ $lid:x$ >> in
  fun
  [ [] -> assert False
  | [x::xs] -> List.fold_left apply1 <:expr< $lid:x$ >> xs];

value ows = rule [LIST0 (' ' | '\t')]   (* [O]ptional [W]hite [S]pace *);

value n  = rule ['\n'];                   (* [N]ewline required  *)
value ns = rule [LIST0 n SEP ows -> ()];  (* [N]ewline[S] allowed *)
value no = rule [ !n ];                   (* [NO] newlines allowed *)

value ws fn =                        (* [W]hite [S]pace *)
  if fn == ns
  then rule [LIST1 (' ' | '\t' | n)] 
  else rule [LIST1 (' ' | '\t')] 
;
value lc = rule ['a'..'z' | '_']
;
value uc = rule ['A'..'Z']
;
value word_char = rule [lc|uc]
;
value string_of_chars chars =
  let len = List.length chars
  in let str = String.create len
  in let rec loop n =
    fun [ [] -> str | [c::cs] -> do { str.[n] := c; loop (n+1) cs } ]
  in loop 0 chars
;
value lid = rule [lc:c; LIST0 word_char: cs; ows -> string_of_chars [c::cs]]
;
value expr fn = rule [LIST1 lid SEP (ws fn) : xs -> fun_app xs]
;
value binding fn = rule [lid:x; fn; "="; ows; expr fn: e; n -> (x,e)]
;
(*    multibind fn = rule ["let"; n; LIST0 binding: xes; "in"; fn -> xes]
and *)
value str_item = rule [
  | binding no: (x,e) -> <:str_item< value $lid:x$ = $e$ >>
(*  | multibind:xes; no -> 
*) ]
;
value implem = rule [ns; LIST0 str_item SEP ns : decls; ns; EOI -> decls]
;
