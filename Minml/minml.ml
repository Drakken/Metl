(* 
 * Copyright 2012 Daniel S. Bensen
 * See LICENSE for details.
 *)

let sum ns = List.fold_left (+) 0 ns

let with_open_in file f =
  let handle = open_in file in
    try
      let x = f handle in close_in handle; x
    with
      x -> close_in handle; raise x

let input_file infile =
  let f handle =
    let rec loop lines =
      try loop (input_line handle :: lines)
      with End_of_file -> List.rev lines
    in loop []
  in
  String.concat "\n" (with_open_in infile f)

module MS = MetlStringAux.Aux

let process infile =
  let text = input_file infile in
  let metlbuf = { MS.str=text; MS.length = String.length text; MS.n=0 }
  in
  match Rules.implem metlbuf with
  | None -> print_endline "No decls."
  | Some (decls,metlbuf) ->
     Printf.printf "metlbuf.n = %d\n" metlbuf.MS.n;
     List.iter Camlp4.PreCast.Printers.OCaml.print_implem decls

let () =
  Camlp4.Register.enable_ocaml_printer ();
  match Sys.argv with 
  | [||] -> assert false
  | [|_|] -> failwith "Please enter infile."
  | [|_;infile|] -> process infile
  | args -> failwith (string_of_int (Array.length args) ^ " args.")
