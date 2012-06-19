
open Camlp4.PreCast

type   expr  = Ast.expr
module Entry = Gram.Entry

module type T = sig
  type ast
  val parser_expr: Ast.patt -> ast -> expr -> expr -> expr
  val next: expr
  val eoi: expr
  val parsr: ast Entry.t
end

