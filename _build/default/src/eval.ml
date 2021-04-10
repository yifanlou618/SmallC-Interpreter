open SmallCTypes
open EvalUtils
open TokenTypes
open Builtins

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let assoc_opt k t =
  if List.mem_assoc k t then
    Some (List.assoc k t)
  else
    None

(* Insert value into environment and raise Declare error with passed message *)
let insert_val (k: string) (v: value) (env: environment) (msg: string): environment = failwith "Unfinished"

(* Helper function useful for inserting primitive values into the environment *)
let insert_primitive k v typ env =
  failwith "Unfinished"

(* Get value from environment and raise DeclareEror if it is not found. *)
let get_val env k = failwith "Unfinished"

(* Function references *)
let funcs = ref []

(* Used to access functions outside module. *)
let get_funcs () = !funcs

let reset_funcs () = funcs := []

let add_func name typ params body =
  if (List.mem_assoc name !funcs) then
    raise (DeclareError ("Function " ^ name ^" already exists"))
  else
    funcs := (name, (typ, params, body))::!funcs

let get_func name = List.assoc name !funcs

let get_type expr = 
  match expr with
  | Int(i) -> Int_Type
  | Bool(b) -> Bool_Type
  | _ -> raise (TypeError("expect int, bool"))
;;

let is_thunk (env: environment) (id: string): bool = failwith "Unfinished"

(* checks that value equals expected data_type fails with TypeError otherwise *)
let check_type (expect: data_type) (v: value): value = failwith "Unfinished"

let rec environment_find env a = 
  match env with
  | [] -> failwith "environment empty"
  | (b, c) :: t -> if (a = b) then 
                    c
                  else
                    (environment_find t a)
;;

let rec environment_search env a = 
  match env with
  | [] -> false
  | (b, c) :: t -> if (a = b) then
                    true
                  else
                    (environment_search t a)
;;

let rec find_ret_value env =
  match env with
  | h :: t -> match h with
              | ("~ret", v) -> v
              | (a, b) -> find_ret_value t
;;

(* Creates thunks for each argument and inserts them into new environment used to evaluate function body.
    Fails if too many or too few arguments and "if" parameters. *)
let rec new_func_scope (params: parameter list) (args: expr list) (curr_env: environment): environment =
  if ((List.length params) != (List.length args)) then
    raise (DeclareError("too many / too few params / args"))
  else
    match params with
    | [] -> []
    | (id, typ) :: t -> (match args with
                        | [] -> raise (DeclareError("too many / too few params / args"))
                        | expr :: tail -> ((id, Thunk_Val(curr_env, expr, typ)) :: (new_func_scope t tail curr_env)))
                        
(* Functions are mutually recursive *)
(* Replace this with your code from P4 and add to it to complete this project *)
and eval_expr env t = 
match t with
| Int i -> Int_Val i
| Bool b -> Bool_Val b
| ID id -> if ((environment_search env id) = false) then
              raise (DeclareError("ID no binding"))
            else
              (match (environment_find env id) with
              | Int_Val i -> Int_Val i
              | Bool_Val b -> Bool_Val b
              | Thunk_Val(newenv, expr, Int_Type) -> (match (eval_expr newenv expr) with
                                                    | Int_Val i -> Int_Val i
                                                    | Bool_Val b -> raise (TypeError("expect int got bool"))
                                                    | _ -> raise (TypeError("no match")))
              | Thunk_Val(newenv, expr, Bool_Type) -> match (eval_expr newenv expr) with
                                                    | Int_Val i -> raise (TypeError("exprct bool got int"))
                                                    | Bool_Val b -> Bool_Val b
                                                    | _ -> raise (TypeError("no match"))
              | _ -> (environment_find env id))

| Add (expr1, expr2) -> (match (eval_expr env expr1) with
                        | Bool_Val b1 -> raise (TypeError("Add left boolean"))
                        | Int_Val i1 -> (match (eval_expr env expr2) with
                                        | Int_Val i2 -> Int_Val (i1 + i2)
                                        | Bool_Val b2 -> raise (TypeError("Add right boolean"))))
                        
| Sub (expr1, expr2) -> (match (eval_expr env expr1) with
                        | Bool_Val b1 -> raise (TypeError("Sub left boolean"))
                        | Int_Val i1 -> (match (eval_expr env expr2) with
                                        | Int_Val i2 -> Int_Val (i1 - i2)
                                        | Bool_Val b2 -> raise (TypeError("Sub right boolean"))))
                        
| Mult (expr1, expr2) -> (match (eval_expr env expr1) with
                        | Bool_Val b1 -> raise (TypeError("Mult left boolean"))
                        | Int_Val i1 -> (match (eval_expr env expr2) with
                                        | Int_Val i2 -> Int_Val (i1 * i2)
                                        | Bool_Val b2 -> raise (TypeError("Mult right boolean"))))
                        
| Div (expr1, expr2) -> (match (eval_expr env expr1) with
                        | Bool_Val b1 -> raise (TypeError("Div left boolean"))
                        | Int_Val i1 -> (match (eval_expr env expr2) with
                                        | Int_Val i2 -> if (i2 = 0) then 
                                                          raise (DivByZeroError)
                                                        else
                                                          Int_Val (i1 / i2)
                                        | Bool_Val b2 -> raise (TypeError("Div right boolean"))))
            
| Pow (expr1, expr2) -> (match (eval_expr env expr1) with
                        | Bool_Val b1 -> raise (TypeError("Pow left boolean"))
                        | Int_Val i1 -> (match (eval_expr env expr2) with
                                        | Int_Val i2 -> Int_Val (int_of_float (((float_of_int i1) ** (float_of_int i2))))
                                        | Bool_Val b2 -> raise (TypeError("Pow right boolean"))))

| Greater (expr1, expr2) -> (match (eval_expr env expr1) with
                            | Bool_Val b1 -> raise (TypeError("Greater left boolean"))
                            | Int_Val i1 -> (match (eval_expr env expr2) with
                                            | Int_Val i2 -> Bool_Val (i1 > i2)
                                            | Bool_Val b2 -> raise (TypeError("Greater right boolean"))))

| Less (expr1, expr2) -> (match (eval_expr env expr1) with
                        | Bool_Val b1 -> raise (TypeError("Less left boolean"))
                        | Int_Val i1 -> (match (eval_expr env expr2) with
                                        | Int_Val i2 -> Bool_Val (i1 < i2)
                                        | Bool_Val b2 -> raise (TypeError("Less right boolean"))))

| GreaterEqual (expr1, expr2) -> (match (eval_expr env expr1) with
                                  | Bool_Val b1 -> raise (TypeError("GreaterEqual left boolean"))
                                  | Int_Val i1 -> (match (eval_expr env expr2) with
                                                  | Int_Val i2 -> Bool_Val (i1 >= i2)
                                                  | Bool_Val b2 -> raise (TypeError("GreaterEqual right boolean"))))

| LessEqual (expr1, expr2) -> (match (eval_expr env expr1) with
                              | Bool_Val b1 -> raise (TypeError("LessEqual left boolean"))
                              | Int_Val i1 -> (match (eval_expr env expr2) with
                                              | Int_Val i2 -> Bool_Val (i1 <= i2)
                                              | Bool_Val b2 -> raise (TypeError("LessEqual right boolean"))))

| Equal (expr1, expr2) -> (match (eval_expr env expr1) with
                          | Bool_Val b1 -> (match (eval_expr env expr2) with
                                            | Int_Val i2 -> raise (TypeError("Equal right int"))
                                            | Bool_Val b2 -> Bool_Val (b1 = b2))
                          | Int_Val i1 -> (match (eval_expr env expr2) with
                                          | Int_Val i2 -> Bool_Val (i1 = i2)
                                          | Bool_Val b2 -> raise (TypeError("Equal right boolean"))))

| NotEqual (expr1, expr2) -> (match (eval_expr env expr1) with
                              | Bool_Val b1 -> (match (eval_expr env expr2) with
                                                | Int_Val i2 -> raise (TypeError("NotEqual right int"))
                                                | Bool_Val b2 -> Bool_Val (b1 != b2))
                              | Int_Val i1 -> (match (eval_expr env expr2) with
                                              | Int_Val i2 -> Bool_Val (i1 != i2)
                                              | Bool_Val b2 -> raise (TypeError("NotEqual right boolean"))))

| Or (expr1, expr2) -> (match (eval_expr env expr1) with
                        | Int_Val i1 -> raise (TypeError("Or left int"))
                        | Bool_Val b1 -> (match (eval_expr env expr2) with
                                          | Bool_Val b2 -> Bool_Val (b1 || b2)
                                          | Int_Val i2 -> raise (TypeError("Or right int"))))

| And (expr1, expr2) -> (match (eval_expr env expr1) with
                        | Int_Val i1 -> raise (TypeError("And left int"))
                        | Bool_Val b1 -> (match (eval_expr env expr2) with
                                          | Bool_Val b2 -> Bool_Val (b1 && b2)
                                          | Int_Val i2 -> raise (TypeError("And right int"))))

| Not expr -> (match (eval_expr env expr) with
              | Int_Val i -> raise (TypeError("Not int"))
              | Bool_Val b -> Bool_Val (not b))

| FunctionCall(n, a) when is_builtin n -> call_builtin n a env; (Int_Val 0)

| FunctionCall (id, exprlst) -> let funct = get_func id in
                                (match funct with
                                | (Int_Type, paramlst, body) -> if ((List.length paramlst) != (List.length exprlst)) then
                                                                  raise (DeclareError("too many / too few params / args"))
                                                                else 
                                                                  let newenv = (new_func_scope paramlst exprlst env) in
                                                                  let env2 = (eval_stmt newenv body) in
                                                                  (environment_find env2 "~ret")
                                | (Bool_Type, paramlst, body) -> if ((List.length paramlst) != (List.length exprlst)) then
                                                                  raise (DeclareError("too many / too few params / args"))
                                                                else 
                                                                  let newenv = (new_func_scope paramlst exprlst env) in
                                                                  let env2 = (eval_stmt newenv body) in
                                                                  (environment_find env2 "~ret")
                                | _ -> raise (TypeError("wrong function type")))

(* Replace this with your code from P4 and add to it to complete this project *)
and eval_stmt env s = 
match s with
    | NoOp -> env

    | Seq (seq1, seq2) -> (eval_stmt (eval_stmt env seq1) seq2)

    | Declare (t, id) -> (if ((environment_search env id) = false) then
                            (match t with
                            | Int_Type -> ((id, Int_Val(0)) :: env)
                            | Bool_Type -> ((id, Bool_Val(false)) :: env))
                          else
                            (match t with
                            | Int_Type -> ((id, (environment_find env id)) :: env)
                            | Bool_Type -> ((id, (environment_find env id)) :: env)))

    | Assign (id, expr) -> (if ((environment_search env id) = true) then
                              (match (environment_find env id) with
                              | Int_Val i1 -> (match (eval_expr env expr) with
                                              | Int_Val i2 -> ((id, (eval_expr env expr)) :: env)
                                              | Bool_Val b2 -> raise (TypeError("Assign right boolean")))
                              | Bool_Val b1 -> (match (eval_expr env expr) with
                                              | Bool_Val b2 -> ((id, (eval_expr env expr)) :: env)
                                              | Int_Val i2 -> raise (TypeError("Assign right int"))))
                            else
                              raise (DeclareError("ID not found in environment")))

    | If (expr, if_body, else_body) -> (match (eval_expr env expr) with
                                      | Bool_Val b -> (if (b = true) then
                                                        (eval_stmt env if_body)
                                                      else
                                                        (eval_stmt env else_body))
                                      | Int_Val i -> raise (TypeError("If expr is int but not bool")))

    | For (id, expr1, expr2, body) -> (match (eval_expr env expr1) with
                                      | Int_Val i1 -> (match (eval_expr env expr2) with
                                                      | Int_Val i2 -> (if ((eval_expr env expr1) <= (eval_expr env expr2)) then
                                                                        (eval_stmt 
                                                                          (eval_stmt 
                                                                            (eval_stmt ((id, eval_expr env expr1) :: env) 
                                                                              (Assign(id, expr1))) body) 
                                                                                (For(id, (Add(expr1, Int(1))), expr2, body)))
                                                                      else
                                                                        ((id, (eval_expr env expr1)) :: env)))
                                                      | Bool_Val b2 -> raise (TypeError("need int not bool"))
                                      | Bool_Val b1 -> raise (TypeError("need int not boll")))

    | While (expr, body) -> (match (eval_expr env expr) with
                            | Bool_Val b -> (match b with
                                            | false -> env
                                            | true -> (eval_stmt (eval_stmt env body) (While(expr, body))))
                            | Int_Val i -> raise (TypeError("While condition wrong")))

    | Print (expr) -> (match (eval_expr env expr) with
                      | Int_Val i -> (print_output_int i); 
                                      (print_output_newline());
                                      env
                      | Bool_Val b -> (print_output_bool b);
                                      (print_output_newline());
                                      env)
    | Return (expr) -> (("~ret", (eval_expr env expr)) :: env)
    
    | FunctionDecl (name, typ, params, stmt) -> if (name = "main") then
                                                  (eval_stmt env stmt)
                                                else
                                                  ((add_func name typ params stmt);
                                                  env)
    
;;
