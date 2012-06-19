
let delim_chars = "([{}])"

let symbol_chars = "~!@#$%^&*-+=|:<>,.?/"

let is_alpha c = c='_' || (c>='A' && c<='Z') || (c>='a' && c<='z')

let is_dig c = c>='0' && c<='9'

let is_word_char c = is_alpha c || is_dig c

let is_symbol_char c = String.contains symbol_chars c
let  is_delim_char c = String.contains  delim_chars c
