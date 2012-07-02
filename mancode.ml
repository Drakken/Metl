
let digit = rule ['0'..'9': d -> Char.code d - Char.code '0']

let sum = rule [digit: x; "+"; digit: y -> (x,y)]

let print_sum = rule [ sum: (x,y) -> Printf.printf "%d + %d = %d\n" x y (x+y)]

let _ = print_sum (MetlStringAux.Aux.make_buffer "2+2")

