(* 
 * Copyright 2012 Daniel S. Bensen
 * See LICENSE for details.
 *)

open Printf
;
open Camlp4.PreCast
;
value debugf: ('a, out_channel, unit) format -> 'a = ()
;
value got_list xs str = debugf "Got %d %s.\n" (List.length xs) str
;
value _loc = Loc.mk "rules.ml"
;
value fun_app =
  let apply1 ef ex = <:expr< $ef$ $ex$ >> in
  fun
  [ [] -> assert False
  | [e::es] -> List.fold_left apply1 e es];

value ows = rule [LIST0 (' ' | '\t')]   (* [O]ptional [W]hite [S]pace *);

value n  = rule ['\n'];                   (* [N]ewline required  *)
value n'  = rule ['\n' -> do { debugf "Got an n.\n"; '\n' } ];
value ns = rule [LIST0 n SEP ows: ns -> got_list ns "ns"];  (* [N]ewline[S] allowed *)
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
value lid = rule [
  lc:c; LIST0 word_char: cs ->
    let str = string_of_chars [c::cs]
    in do { debugf "Got lid %s." str; str }
];
value not_quote = rule [!'"'; ANY]
;
value count1 p = rule [ LIST1 p SEP ' ': xs ->
      do { debugf "Got %d p's." (List.length xs); exit 1 } ]
;
value rec
     expr fn = rule [
  | (LIST1 closed SEP (ws fn)): xs -> do { got_list xs "closeds"; fun_app xs }
] and
     closed = rule [
  | lid:x -> <:expr< $lid:x$ >>
  | '"'; LIST0 not_quote: chars; '"' -> <:expr< $str: string_of_chars chars$ >>
  | '('; expr ns: e; ')' -> e
];
value parens = rule [ "()" -> print_endline "Got parens."
];
value lpatt = rule [
  | parens -> do { debugf "Got parens.\n";  <:patt< () >> }
  | lid:x -> <:patt< $lid:x$ >>
];
value binding fn = rule [lpatt:pat; ows; "="; ows; expr fn: e; n' -> (pat,e)]
;
(*    multibind fn = rule ["let"; n; LIST0 binding: xes; "in"; fn -> xes]
and *)
value str_item = rule [
  | binding no: (x,e) -> <:str_item< value $pat:x$ = $e$ >>
(*  | multibind:xes; no -> 
*) ]
;
value x = rule [ 'x' ]
;
value implem = rule [ns; LIST0 str_item SEP ns : decls; ns -> decls]
;

