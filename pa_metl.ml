
ENTRIES
  metl args
END

EXTEND Camlp4.PreCast.Gram

  ;implem: LAST [[ "rule"; x = rule; xs = SELF -> x::xs
                 | `EOI -> [] ]]

  ;rule: [[ f = lid; args = LIST0 [a = arg -> ...];
	    "="; b = body -> rule_expr f args b]]

  ;body:
    [ "action body" [ b = action_body -> [b] ]
    [ "parser body" [ p  = parser -> [(p,<:expr< () >>)] ] ]

  ;action_body:
    [ "alt body" [ bs = LIST1 [b = NEXT -> b] SEP "/"-> bs ]
    |  "action"  [ p  = parser; "->"; "{"; e = Camlp4.Precast.expr; "}"; -> (p,e) ] ]

  ;parser:
    [ "alt parser" [ ps = LIST1 [p = NEXT -> p] SEP "/"-> Alts ps ]
    |  "sequence"  [ p1 = SELF; ","; p2 = NEXT -> Seq (p1,p2) ]
    | "binding" NONA [ p = SELF; ":"; x = lid -> Binding p x
    | "unary" NONA [ "!"; p = NEXT ->  Deny  p
                 | "&"; p = NEXT -> Affirm p
                 | "?"; e = expr ->  Test  e
                 | p = NEXT; "*" -> Multi0 p
                 | p = NEXT; "+" -> Multi1 p
                 | p = NEXT; "?" ->  Opt  p ] 
    | "closed"   [ "("; p = SELF; ")" -> p
                 | p = lid -> Lid p ] ]

(* fun op e -> <:expr< $op$ $b$ (fun $x$ -> $e$) >> *) ]

(*
rule foo :o = bar:x, o ...
=>
let foo = fun o -> fun gensym1 ->
  bar gensym1 >>=
    fun (x,_) as gensym2 -> o gensym2 ...
*)
