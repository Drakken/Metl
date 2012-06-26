
open Camlp4.PreCast;

value gram_entries = Gram.Entry.mk "gram_entries";
Gram.Entry.clear gram_entries;

EXTEND Gram
  Syntax.implem: LAST [["ENTRIES"; x = gram_entries -> x ]];
  gram_entries: [ "top"
   [ name = LIDENT; (items,status) = SELF ->
       let decl = <:str_item< value $lid:name$ = Gram.Entry.mk $str:name$ >>
       and clear = <:str_item< Gram.Entry.clear $lid:name$ >>
       in ([decl;clear::items], status)
   | "END"; x = Syntax.implem -> x ]];
END;
