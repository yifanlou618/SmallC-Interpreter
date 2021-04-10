open TokenTypes

let regexp_match : (token * Str.regexp * int) list =
  List.map (fun (token, string, length) -> (token, (Str.regexp string), length))
      [(Tok_For, "for", 3);
      (Tok_From, "from", 4);
      (Tok_To, "to", 2);
      (Tok_While, "while", 5);
      (Tok_Int_Type, "int", 3);
      (Tok_Bool_Type, "bool", 4);
      (Tok_Sub, "\\-", 1);
      (Tok_Semi, ";", 1);
      (Tok_RParen, ")", 1);
      (Tok_RBrace, "}", 1);
      (Tok_Print, "printf", 6);
      (Tok_Pow, "\\^", 1);
      (Tok_Add, "\\+", 1);
      (Tok_Or, "||", 2);
      (Tok_NotEqual, "!=",  2);
      (Tok_Not, "!", 1);
      (Tok_Mult, "\\*", 1);
      (Tok_Main, "main", 4);
      (Tok_LessEqual, "<=", 2);
      (Tok_Less, "<", 1);
      (Tok_LParen, "(", 1);
      (Tok_LBrace, "{", 1);
      (Tok_If, "if", 2);
      (Tok_GreaterEqual, ">=", 2);
      (Tok_Greater, ">", 1);
      (Tok_Equal, "==", 2);
      (Tok_Else, "else", 4);
      (Tok_Div, "\\/", 1);
      (Tok_Assign, "=", 1);
      (Tok_And, "&&", 2);
      (Tok_Comma, ",", 1);
      (Tok_Return, "return", 6);
      (EOF, "EOF", 3)]
;;

let rec single_match string index regexmatch =
  match regexmatch with
    | [] -> None
    | (token, regex, length) :: t -> 
      if (Str.string_match regex string index) then 
        Some (token, length)
      else 
        (single_match string index t)


let rec tok_help index string help_string =
  if index >= String.length string then
    (List.rev @@ (EOF :: help_string))
      
  else if (Str.string_match (Str.regexp "-?[0-9]+") string index) then
    (tok_help (index + (String.length (Str.matched_string string))) (string) 
              ((Tok_Int (int_of_string (Str.matched_string string))) :: help_string))
      
  else if ((Str.string_match (Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*") string index) && (not(List.mem (Str.matched_string string) 
            (["while";"printf";"true";"false";"int";"bool";"main";"for";"if";"else";"return";"from";"to";"EOF"])))) then
    (tok_help (index + (String.length (Str.matched_string string))) (string) 
              ((Tok_ID (Str.matched_string string)) :: help_string))
      
  else if (Str.string_match (Str.regexp "true\\|false") string index) then
    (tok_help (index + (String.length (Str.matched_string string))) (string) 
              ((Tok_Bool (bool_of_string (Str.matched_string string))) :: help_string))
      
  else if (Str.string_match (Str.regexp "[ \n\t]+") string index) then
    (tok_help (index + 1) (string) (help_string))
      
  else
    let m = (single_match string index regexp_match) in
      match m with
        | Some (token, length) -> (tok_help (index + length) (string) (token :: help_string))
        | None -> raise (InvalidInputException "tokenize")

let tokenize input =
  tok_help 0 input []
;;