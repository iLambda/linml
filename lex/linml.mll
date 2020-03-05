{
  open Lexing
  open Parser
  open Utils

  (*
   *  HELPERS
   *)
  (* Goto next line *)
  let next_line_and f lexbuf =
    Lexing.new_line lexbuf;
    f lexbuf
  (* Report a lexing error *)
  let error lexbuf msg = Error.errorb lexbuf msg
  (* Parse a number *)
  let mk_num i = Int64.of_string i


  (*
   *  KEYWORDS
   *)
  (* The list of keywords *)
  let keywords = [
    (* "let", KEYWORD_LET; *)
    "give", KEYWORD_GIVE;
    "in", KEYWORD_IN;
    "fun", KEYWORD_FUN;
    "match", KEYWORD_MATCH;
    "with", KEYWORD_WITH;
    "return", KEYWORD_RETURN;
    "end", KEYWORD_END;
    "either", KEYWORD_EITHER;
    "refute", KEYWORD_ZERO;
    "forall", KEYWORD_FORALL;
  ]
  (* Try to convert a string to a keyword *)
  let keyword_of_string str = List.assoc str keywords
}

(*
 *  REGEXPS
 *)
(* Special characters *)
let newline = ('\010' | '\013' | "\013\010")
let blank = [ ' ' '\t' ]
let comment = "(*"

(* Literals *)
let lit_digit_base10 = [ '0'-'9' ]
let lit_digit_base16 = [ '0'-'9' 'a'-'f' 'A'-'F' ]
let lit_integer = ("0x" lit_digit_base16+) 
                | (lit_digit_base10+)

let lit_stringatom    = ['\x20'-'\x7E']#['"']
let lit_stringescape    = "n" | "b" | "t" | "r" | "\\" | "\""
let lit_stringescapenum = ("0x" lit_digit_base16 lit_digit_base16)
                        | (['0'-'1'] lit_digit_base10 lit_digit_base10) 
                        | ("2" ['0'-'4'] lit_digit_base10) 
                        | ("25"['0'-'5'])

(* Identifiers & tags *)
let char_lower = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let char_nounder_lower = ['a'-'z' '\223'-'\246' '\248'-'\255' ]
let char_upper = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let char_all = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let identifier = char_nounder_lower | (char_lower char_all+)
let tag = char_upper char_all*

(* Operators *)

(* let operator_char = ['!' '$' '&' '*' '#' '+' '-' '.' '/' ':'  '=' '?' '@' '^' '|' '~' ]
let operator_prefix = (['!'] operator_char +) | (['?' '~' '#'] operator_char* )
let operator_lv0 = (['=' '|' '&' '<' '>'] operator_char+) | (['$'] operator_char* ) 
let operator_lv1 = (['@' '^'] operator_char* ) 
let operator_lv2 = (['-'] operator_char+ ) | (['+'] operator_char+) 
let operator_lv3 = (['/' '%'] operator_char* ) | (['*'] operator_char+)
let operator_lv4 = ("**") operator_char*   *)

(* 
 *  RULES
 *)
rule main = parse 

  (* Layout *)
  | newline         { next_line_and main lexbuf }
  | comment         { comments (lexeme_start_p lexbuf) lexbuf; main lexbuf }
  | blank+          { main lexbuf }

  (* Special identifiers & tags *)
  | lit_integer as n  { INTEGER (mk_num n, n) }

  (* Identifiers, tags and keywords  *)
  | tag as id      { TAG (id, lexeme_start_p lexbuf, lexeme_end_p lexbuf) }
  | identifier as id
    { 
      (* Try find keyword *)
      try keyword_of_string id
      (* Not a keyword*)
      with Not_found -> 
        (* Make a new identifier *)
        IDENTIFIER (id, lexeme_start_p lexbuf, lexeme_end_p lexbuf)
    }

  (* Literals *)

  (* Operators *)
  | "=>"            { OPERATOR_FATARROW }
  | "-o"            { OPERATOR_LOLLIPOP }
  | ":>"            { OPERATOR_INJECT }
  | "<:"            { OPERATOR_EXTRACT }

  (* Punctuation *)
  (* | ';'             { PUNCTUATION_SEMICOLON } *)
  | '|'             { PUNCTUATION_BAR }
  | '_'             { PUNCTUATION_UNDERSCORE }
  | '('             { PUNCTUATION_LPAREN }
  | ')'             { PUNCTUATION_RPAREN }
  | '<'             { PUNCTUATION_LANGLE }
  | '>'             { PUNCTUATION_RANGLE }
  | '!'             { PUNCTUATION_BANG }
  | '*'             { PUNCTUATION_STAR }
  | '&'             { PUNCTUATION_AND }
  | '+'             { PUNCTUATION_PLUS }
  | '-'             { PUNCTUATION_MINUS }
  | ':'             { PUNCTUATION_COLON }
  | ','             { PUNCTUATION_COMMA }
  | '='             { PUNCTUATION_EQUAL }
  | '.'             { PUNCTUATION_DOT }

  (* Infix&prefix identifiers *)
  (* | operator_lv4 as id { IDENTIFIER_OP4 (id, lexeme_start_p lexbuf, lexeme_end_p lexbuf) }
  | operator_lv3 as id { IDENTIFIER_OP3 (id, lexeme_start_p lexbuf, lexeme_end_p lexbuf) } 
  | operator_lv2 as id { IDENTIFIER_OP2 (id, lexeme_start_p lexbuf, lexeme_end_p lexbuf) }
  | operator_lv1 as id { IDENTIFIER_OP1 (id, lexeme_start_p lexbuf, lexeme_end_p lexbuf) }
  | operator_lv0 as id { IDENTIFIER_OP0 (id, lexeme_start_p lexbuf, lexeme_end_p lexbuf) }
  | operator_prefix as id { IDENTIFIER_PREFIX (id, lexeme_start_p lexbuf, lexeme_end_p lexbuf) } *)

  (* Error *)
  | eof             { EOF }
  | _               { error lexbuf "Unexpected character." }

and comments openingp = parse
  | "(*"        { comments (lexeme_start_p lexbuf) lexbuf; comments openingp lexbuf }
  | "*)"        { () }
  | newline     { next_line_and (comments openingp) lexbuf }
  | eof         { Error.error [ (openingp, lexeme_start_p lexbuf) ] "Unterminated comment." }
  | _           { comments openingp lexbuf }

