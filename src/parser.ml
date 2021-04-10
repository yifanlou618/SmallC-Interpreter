open SmallCTypes
open Utils
open TokenTypes


(* Parsing helpers *)

let tok_list = ref []

(* Returns next token in the list. *)
let lookahead () : token =
  match !tok_list with
  | [] -> raise (InvalidInputException "no tokens")
  | h :: t -> h

(* Matches the top token in the list. *)
let consume (a : token) : unit  =
  match !tok_list with
  | h :: t when a = h -> tok_list := t
  | _ -> raise (InvalidInputException (Printf.sprintf "%s is invalid token, input is %s" 
                                      (string_of_token a)
                                      (string_of_list string_of_token !tok_list)))

(* Parsing *)

let rec orExpr () =
  let exp_one = andExpr() in
  let t = lookahead() in
  match t with
  | Tok_Or -> consume Tok_Or;
    let exp_two = orExpr() in 
    Or(exp_one, exp_two)
  | _ -> exp_one

and andExpr () =
  let exp_one = equalityExpr() in
  let t = lookahead() in
  match t with
  | Tok_And -> consume Tok_And;
    let exp_two = andExpr() in 
    And(exp_one, exp_two)
  | _ -> exp_one

and equalityExpr () =
  let exp_one = relationalExpr() in
  let t = lookahead() in
  match t with
  | Tok_Equal -> consume Tok_Equal;
    let exp_two = equalityExpr() in 
    Equal(exp_one, exp_two)
  | Tok_NotEqual -> consume Tok_NotEqual;
    let exp_two = equalityExpr() in 
    NotEqual(exp_one, exp_two)
  | _ -> exp_one

and relationalExpr () =
  let exp_one = additiveExpr() in
  let t = lookahead() in
  match t with
  | Tok_Less -> consume Tok_Less;
    let exp_two = relationalExpr() in 
    Less(exp_one, exp_two)
  | Tok_Greater -> consume Tok_Greater;
    let exp_two = relationalExpr() in 
    Greater(exp_one, exp_two)
  | Tok_LessEqual -> consume Tok_LessEqual;
    let exp_two = relationalExpr() in 
    LessEqual(exp_one, exp_two)
  | Tok_GreaterEqual -> consume Tok_GreaterEqual;
    let exp_two = relationalExpr() in 
    GreaterEqual(exp_one, exp_two)
  | _ -> exp_one

and additiveExpr () =
  let exp_one = multiplicativeExpr() in
  let t = lookahead() in
  match t with
  | Tok_Add -> consume Tok_Add;
    let exp_two = additiveExpr() in 
    Add(exp_one, exp_two)
  | Tok_Sub -> consume Tok_Sub;
    let exp_two = additiveExpr() in 
    Sub(exp_one, exp_two)
  | _ -> exp_one


and multiplicativeExpr () =
  let exp_one = powExpr() in
  let t = lookahead() in
  match t with
  | Tok_Mult -> consume Tok_Mult;
    let exp_two = multiplicativeExpr() in 
    Mult(exp_one, exp_two)
  | Tok_Div -> consume Tok_Div;
    let exp_two = multiplicativeExpr() in 
    Div(exp_one, exp_two)
  | _ -> exp_one


and powExpr () =
  let exp_one = unaryExpr() in
  let t = lookahead() in
  match t with
  | Tok_Pow -> consume Tok_Pow;
    let exp_two = powExpr() in 
    Pow(exp_one, exp_two)
  | _ -> exp_one

and unaryExpr () =
  let t = lookahead() in
  match t with
  | Tok_Not -> consume Tok_Not;
    Not(unaryExpr())
  | _ -> primaryExpr()

and primaryExpr () =
  let t = lookahead() in
  match t with
  | Tok_Int (i) -> consume (Tok_Int i);
    Int (i)
  | Tok_Bool (b) -> consume (Tok_Bool b);
    Bool (b)
  | Tok_ID (id) ->  consume (Tok_ID id); 
                    (let tt = lookahead() in 
                    match tt with
                    | Tok_LParen -> consume Tok_LParen;
                                    (let ttt = lookahead() in 
                                      match ttt with
                                    | Tok_RParen -> consume Tok_RParen;
                                                    FunctionCall(id, [])
                                    | _ -> let expr = orExpr() in
                                          let exprlist = exprList() in
                                          FunctionCall(id, (expr :: exprlist)))
                    | _ -> ID (id))
  | Tok_LParen -> consume Tok_LParen;
    let exp = orExpr() in
    consume Tok_RParen;
    exp

and exprList() =
  let t = lookahead() in
  match t with
  | Tok_Comma -> consume Tok_Comma;
                  (let tt = lookahead() in
                  match tt with
                  | Tok_Comma -> raise (InvalidInputException ("too many comma"))
                  | _ -> let expr = orExpr() in
                          let exprlist = exprList() in
                          (expr :: exprlist))
  | _ -> consume Tok_RParen; []
;;

let rec parse_expr toks =
  tok_list := toks;
  (!tok_list, orExpr())
;;

let rec stmt_help() =
  let exp_one = stmtOptions() in 
  if (exp_one = NoOp) then 
    NoOp 
  else
    let t = lookahead() in
    match t with
    | Tok_Int_Type -> Seq(exp_one, stmt_help())
    | Tok_Bool_Type -> Seq(exp_one, stmt_help())
    | (Tok_ID id) -> Seq(exp_one, stmt_help())
    | Tok_If -> Seq(exp_one, stmt_help())
    | Tok_For -> Seq(exp_one, stmt_help())
    | Tok_While -> Seq(exp_one, stmt_help())
    | Tok_Print -> Seq(exp_one, stmt_help())
    | Tok_Return -> Seq(exp_one, stmt_help())
    | _ -> Seq(exp_one, NoOp)

and stmtOptions() = 
  let t = lookahead() in
  match t with
  | Tok_Int_Type -> consume Tok_Int_Type;
                    let dcl = Declare(Int_Type, declare_statement()) in 
                    consume Tok_Semi;
                    dcl
  | Tok_Bool_Type -> consume Tok_Bool_Type;
                    let dcl = Declare(Bool_Type, declare_statement()) in 
                    consume Tok_Semi;
                    dcl
  | Tok_ID id -> consume (Tok_ID id);
                consume Tok_Assign;
                let value = orExpr() in
                consume Tok_Semi;
                Assign(id, value)
  | Tok_If -> consume Tok_If; 
              consume Tok_LParen;
              let condition = orExpr() in 
              consume Tok_RParen;
              consume Tok_LBrace;
              let statement = stmt_help() in 
              consume Tok_RBrace;
              let else_stmt = else_statement() in
              If(condition, statement, else_stmt)
  | Tok_For -> consume Tok_For;
              consume Tok_LParen;
              let for1 = for_statement() in
              consume Tok_From;
              let exp1 = orExpr() in 
              consume Tok_To;
              let exp2 = orExpr() in
              consume Tok_RParen;
              consume Tok_LBrace;
              let statement = stmt_help() in
              consume Tok_RBrace;
              For(for1, exp1, exp2, statement)
  | Tok_While -> consume Tok_While;
                consume Tok_LParen;
                let condition = orExpr() in 
                consume Tok_RParen;
                consume Tok_LBrace;
                let statement = stmt_help() in 
                consume Tok_RBrace;
                While(condition, statement)
  | Tok_Print -> consume Tok_Print;
                consume Tok_LParen;
                let to_print = orExpr() in 
                consume Tok_RParen;
                consume Tok_Semi;
                Print(to_print)
  | Tok_Return -> consume Tok_Return;
                  let to_return = orExpr() in
                  consume Tok_Semi;
                  Return(to_return)
  | _ -> NoOp

and for_statement() =
  let t = lookahead() in
  match t with
  | Tok_ID id -> consume (Tok_ID id);
    id

and declare_statement() = 
  let t = lookahead() in
  match t with
  | Tok_ID id -> consume (Tok_ID id);
    id

and else_statement() = 
  let t = lookahead() in
  match t with
  | Tok_Else -> consume Tok_Else;
    consume Tok_LBrace;
    let statement = stmt_help() in 
    consume Tok_RBrace; 
    statement
  | _ -> NoOp
;;

let rec parse_stmt toks =
  tok_list := toks;
  (!tok_list, stmt_help())
;;

let rec functionDecl () = 
  let t = lookahead() in
    match t with
    | Tok_Int_Type -> consume Tok_Int_Type;
            (let tt = lookahead() in
            match tt with
            | Tok_Main -> consume Tok_Main;
                          consume Tok_LParen;
                          consume Tok_RParen;
                          consume Tok_LBrace;
                          let stmt = stmt_help() in
                          consume Tok_RBrace;
                          consume EOF;
                          FunctionDecl("main", Int_Type, [], stmt)
            | Tok_ID id -> (consume (Tok_ID id);
                            consume Tok_LParen;
                            (let ttt = lookahead() in match t with
                            | Tok_RParen -> consume Tok_RParen;
                                            consume Tok_LBrace;
                                            let stmt = stmt_help() in
                                            consume Tok_RBrace;
                                            Seq(FunctionDecl(id, Int_Type, [], stmt), functionDecl())
                            | _ -> let param = param_help() in
                                  let paramlist = paramlist_help() in
                                  consume Tok_LBrace;
                                  let stmt = stmt_help() in
                                  consume Tok_RBrace;
                                  Seq(FunctionDecl(id, Int_Type, (param @ paramlist), stmt), functionDecl())))
                          
          (*| _ -> raise (InvalidInputException (Printf.sprintf "%s is invalid token" (string_of_token t)))*))

    | Tok_Bool_Type -> consume Tok_Bool_Type;
            (let tt = lookahead() in
            match tt with
            | Tok_ID id -> consume (Tok_ID id);
                            consume Tok_LParen;
                            (let ttt = lookahead() in
                            match t with
                            | Tok_RParen -> consume Tok_RParen;
                                            consume Tok_LBrace;
                                            let stmt = stmt_help() in
                                            consume Tok_RBrace;
                                            FunctionDecl(id, Bool_Type, [], stmt)
                            | _ -> let param = param_help() in
                                  let paramlist = paramlist_help() in
                                  consume Tok_LBrace;
                                  let stmt = stmt_help() in
                                  consume Tok_RBrace;
                                  Seq(FunctionDecl(id, Bool_Type, (param @ paramlist), stmt), functionDecl()))
            (*| _ -> raise (InvalidInputException "bad match bool")*))
    | _ -> NoOp

  and paramlist_help () =
  let t = lookahead() in 
    match t with
    | Tok_Comma -> consume Tok_Comma;
                    (let tt = lookahead() in
                      match tt with 
                    | Tok_Comma -> raise (InvalidInputException ("too many comma"))
                    | _ -> let param = param_help() in
                            let paramlist = paramlist_help() in
                            (param @ paramlist))
    | _ -> consume Tok_RParen; []
    
  and param_help () =
  let t = lookahead() in 
    match t with
    | Tok_Int_Type -> consume Tok_Int_Type;
                      (let tt = lookahead() in
                        match tt with
                      | Tok_ID id -> consume (Tok_ID id);
                                    (id, Int_Type) :: [])
    | Tok_Bool_Type -> consume Tok_Bool_Type;
                      (let tt = lookahead() in
                        match tt with
                      | Tok_ID id -> consume (Tok_ID id);
                                    (id, Bool_Type) :: [])
    | _ -> []
;;                  
  
let parse_top_level toks =
  tok_list := toks;
  functionDecl()
;;


(* PLEASE UNCOMMENT BASED ON YOUR IMPLEMENTATION *)
(* ONLY ONE LINE SHOULD BE UNCOMMENTED IN EACH FUNCTION *)
let parse_expr_wrapper toks =
    (* UNCOMMENT the following line if you did the parser FUNCTIONALLY *)
    let (_, e) = parse_expr toks in e
    (* UNCOMMENT the following line if you did the parser IMPERATIVELY *)
    (* tok_list := toks; parse_expr () *)

let parse_stmt_wrapper toks =
    (* UNCOMMENT the following line if you did the parser FUNCTIONALLY *)
    let (_, e) = parse_stmt toks in e
    (* UNCOMMENT the following line if you did the parser IMPERATIVELY *)
    (* tok_list := toks; parse_expr () *)

    (*(let t = lookahead() in 
    (match t with
  | EOF -> consume EOF;
          FunctionDecl("main", Int_Type, [], stmt)
  | _ -> raise (InvalidInputException "parse main")))*)

 