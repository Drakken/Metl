(* 
 * Copyright 2012 Daniel S. Bensen
 * See LICENSE for details.
 *)

open Camlp4.PreCast;

value require test str = if not test then failwith str else ();

value nodef str = failwith "The function \""^str^"\" has not been implemented.";

value from_some = fun [ Some x -> x | None -> invalid_arg "None"];

value name_generator ?(n1=1) prefix =
  let n = ref (n1-1) in
  fun suffix -> do { n.val := n.val + 1; "_" ^ prefix ^ "_" ^ string_of_int n.val ^ "_" ^ suffix };

value gensym = name_generator "metl_name";

value identity x = x;

value genbuf = name_generator "metl_buffer";

value _loc = Loc.mk "Metl/Utils";

value none   = <:expr< None >>;

