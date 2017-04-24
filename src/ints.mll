{}

let digit = ['0'-'9']
let space = [' ' '\t' '\n' '\r']*
let comment = '#' [^ '\n' '\r']+ ['\n' '\r']
  
rule next = parse
| eof { None }
| space { next lexbuf }
| digit+ as n { Some (int_of_string n) }
| comment { next lexbuf }
| _ { next lexbuf }

{}
