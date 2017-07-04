(* Laurent Viennot, Inria 2017 *)

{

type cell =
  | Int of int | Float of float | Time of int
  | String of string | Ident of string
  | Empty

type token =
  | Cell of cell | Coma | Eol | Eof

open Lexing

let fail_at lexbuf msg =
  let pos = lexbuf.lex_curr_p in
  let s = Printf.sprintf "%s at line %d, char %d."
    msg pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
  in failwith s

}

let space = [' ' '\t']*
let eol = ('\r'? '\n' | '\n'? '\r')    
let digit = ['0'-'9']
let identchar = [ ^ '\t' '\r' '\n' ',' '"']
let dquote = '"'
let string = [ ^'"' ]*
let coma = ','
let colon = ':'
  
rule token = parse
| space+ { token lexbuf }
| (digit? digit as h) colon (digit digit as m) colon (digit digit as s)
    { let i = int_of_string in Cell (Time (3600 * (i h) + 60 * (i m) + (i s))) }
| (digit+ '.' digit* as w) { Cell (Float (float_of_string w)) } 
| digit+ as w { Cell (Int (int_of_string w)) }
| identchar+ as w { Cell (Ident w) }
| dquote (string as w) dquote { Cell (String w) }
| coma { Coma }
| eol { new_line lexbuf; Eol }
| eof { Eof }
| (_ as c) { failwith (Printf.sprintf "Rows.token: unexpected char: '%c'" c) }

    
{

  let cell_to_string c =
    match c with
      | Int i -> Printf.sprintf "0%d" i
      | Float f -> Printf.sprintf "0%f" f
      | String s -> Printf.sprintf "\"%s\"" s
      | Ident s -> s
      | Time t ->
        let h = t / 3600 in
        let m = (t - 3600 * h) / 60 in
        let s = (t - 3600 * h - 60 * m) in
        Printf.sprintf "%02d:%02d:%02d" h m s
      | Empty -> ""

  let fprint_row f r =
    let first = ref true in
    List.iter (fun c ->
      if !first then first := false else Printf.fprintf f ",";
      Printf.fprintf f "%s" (cell_to_string c)
    ) r;
    Printf.fprintf f "\n";
    ()
        
  let row lexbuf = 
    let rec row acc =
      match token lexbuf with
        | Cell c -> coma (c :: acc)
        | Coma -> row (Empty :: acc)
        | Eol -> List.rev (Empty :: acc)
        | Eof -> []
    and coma acc =
      match token lexbuf with
        | Eof | Eol -> List.rev acc
        | Cell _ -> fail_at lexbuf "Rows: ',' expected.";
        | Coma -> row acc
    in row []
          
  let all lexbuf =
    let cols = row lexbuf in
    let len = List.length cols in
    if len <= 0 then fail_at lexbuf "Rows: no columns!";
    let rec iter rows =
      let r = row lexbuf in
      if r = [] then cols, List.rev rows
      else begin
        if List.length r <> len then
          fail_at lexbuf "Rows: wrong number of fields.";
        iter (r :: rows)
      end
    in iter []

  let read dir table_name =
    let tin = open_in (Filename.concat dir table_name) in
    let lex = Lexing.from_channel tin in
    let cols, rows = all lex in
    close_in tin;
    cols, rows

  let time_of_string s =
    let lex = Lexing.from_string s in
    match token lex with
    | Cell (Time t) -> t
    | _ -> failwith "Rows: time expected"

}
