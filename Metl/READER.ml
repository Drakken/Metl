(* 
 * Copyright 2012 Daniel S. Bensen
 * See LICENSE for details.
 *)

open Camlp4.PreCast

type   expr  = Ast.expr
module Entry = Gram.Entry

module type T = sig
  type pst
  val parser_expr: Ast.patt -> pst -> expr -> expr -> expr
  val next: Loc.t -> expr
  val eoi:  Loc.t -> expr
  val parsr: pst Entry.t
end

