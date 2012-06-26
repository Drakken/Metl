
type metl_buffer = { str: string; length: int; n: int }

let is_no_char c = false

let char c buf =
  if buf.n >= buf.length || buf.str.[buf.n] <> c
  then None
  else Some (c, { (buf) with n = buf.n + 1 })

let range c1 c2 buf =
  if buf.n = buf.length then None
  else let c = buf.str.[buf.n] in
  if c < c1 || c > c2
  then None
  else Some (c, { (buf) with n = buf.n + 1 })

let string_all f str = 
  let len = String.length str in
  let rec loop n =
    n = len || (f str.[n] && loop (n+1))
  in loop 0

let is_word str = 
  String.length str > 0 &&
  not (Base.is_dig str.[0]) &&
  string_all Base.is_word_char str

let is_symbol str = 
  String.length str > 0 &&
  string_all Base.is_symbol_char str

let is_delimitor str =
  String.length str = 1 && Base.is_delim_char str.[0]

(* type t = Word | Symbol | Delim | Mixed *)

let has_token next_char_test tok mb = 
  let len = String.length tok
  in let xlen = mb.length - mb.n
  in xlen >= len
  && tok = String.sub mb.str mb.n len
  && (xlen = len
      || not (next_char_test mb.str.[mb.n + len]))

