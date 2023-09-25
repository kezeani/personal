(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
  | Sqrt
  | Log
  | Deref
  
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Equals
  | LessThan
  | GreaterThan
  | RefAlter
;;

type varid = string ;;
  
type expr =
  | String of string                     (* strings *)
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float                       (* floats *)
  | Bool of bool                         (* booleans *)
  | Ref of expr                          (* refs *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

let empty : varidset =
  SS.empty

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;; 
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with 
  | Var x -> SS.singleton x 
  | Unop (_, expr) -> free_vars expr
  | App (e1, e2)
  | Binop (_, e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Ref exp -> free_vars exp
  | Conditional (e1, e2, e3) -> SS.union 
                                (SS.union (free_vars e1) (free_vars e2)) 
                                (free_vars e3)
  | Fun (var, expr) -> SS.remove var (free_vars expr)
  | Let (var, e1, e2)
  | Letrec (var, e1, e2) -> SS.union (free_vars e1) (SS.remove var (free_vars e2))
  | Raise
  | String _
  | Num _
  | Float _
  | Bool _ 
  | Unassigned -> empty;;
 
  let same_type (e1 : expr) (e2 : expr) : bool = 
  match e1, e2 with 
  | String _, String _
  | Var _, Var _
  | Num _, Num _
  | Float _, Float _
  | Bool _, Bool _
  | Ref _ , Ref _
  | Unop _, Unop _
  | Binop _, Binop _
  | Conditional _, Conditional _
  | Fun _, Fun _
  | Let _, Let _
  | Letrec _, Letrec _
  | Raise, Raise                          
  | Unassigned, Unassigned
  | App _, App _ -> true 
  | _ -> false;;

  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no other variable names
   use the prefix "var". (Otherwise, they might accidentally be the
   same as a generated variable name.) *)
let new_varname =
  let init = ref 0 in 
  fun () ->
    incr init;
    "var" ^ (string_of_int !init);;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  if not (SS.mem var_name (free_vars exp)) then exp else
  match exp with 
  | String _
  | Num _ 
  | Float _
  | Bool _ 
  | Raise 
  | Unassigned -> exp
  | Ref expr -> Ref(subst var_name repl expr)
  | Var x -> if x = var_name then repl else exp
  | Unop (u, expr) -> Unop(u, subst var_name repl expr)
  | Binop (op, e1, e2) -> 
      Binop (op, subst var_name repl e1, subst var_name repl e2)
  | Conditional (e1, e2, e3) -> Conditional (subst var_name repl e1,
                                             subst var_name repl e2,
                                             subst var_name repl e3)
  | Fun (var, expr) -> if var = var_name then exp
                       else if (SS.mem var (free_vars repl)) then
                        let newname = new_varname() in
                        Fun (newname, subst var_name repl
                          (subst var (Var newname) expr))
                       else
                        Fun (var, subst var_name repl expr)
  | Let (var, e1, e2) -> if var = var_name then Let(var, subst var_name repl e1, e2)
  else if (SS.mem var (free_vars repl)) then 
    let newname = new_varname() in 
    Let(newname, subst var_name repl e1, subst var_name repl 
                                         (subst var (Var newname) e2))
  else
    Let (var, subst var_name repl e1, subst var_name repl e2)
  | Letrec (var, e1, e2) -> if var = var_name then exp
                            else if (SS.mem var (free_vars repl)) then
                              let newname = new_varname () in 
                              Letrec (newname, subst var_name repl e1, 
                              subst var_name repl (subst var (Var newname) e2))
                            else 
                              Letrec (var, subst var_name repl e1,
                                           subst var_name repl e2)
  | App (e1, e2) -> App (subst var_name repl e1, subst var_name repl e2)
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with 
  | String x -> x
  | Var x -> x ^ " "
  | Num x -> string_of_int x ^ " "
  | Float x -> string_of_float x ^ " "
  | Bool x -> if x then "true " else "false "
  | Ref x ->  "ref " ^ exp_to_concrete_string x
  | Unop (u, expr) -> let sign = 
                      match u with 
                      | Negate -> "~-" 
                      | Sqrt -> "Sqrt"
                      | Log -> "Log"
                      | Deref -> "!"
                      in sign ^ "(" ^ exp_to_concrete_string expr ^ ") "
  | Binop (op, e1, e2) ->
      let sign =
      match op with
      | Plus -> "+ "
      | Minus -> "- "
      | Times -> "* "
      | Divide -> "/ "
      | Equals -> "= "
      | LessThan -> "< "
      | GreaterThan -> "> "
      | RefAlter -> ":= "
      in exp_to_concrete_string e1 ^ sign ^ exp_to_concrete_string e2
  | Conditional (e1, e2, e3) -> 
    "If " ^ exp_to_concrete_string e1 ^
    "then " ^ exp_to_concrete_string e2 ^ "else " ^
    exp_to_concrete_string e3
  | Fun (var, expr) -> 
    "Fun " ^ var ^ "-> " ^ exp_to_concrete_string expr
  | Let (var, e1, e2) -> 
    "Let " ^ var ^ "= " ^ 
    exp_to_concrete_string e1 ^ "in " ^ exp_to_concrete_string e2
  | Letrec (var, e1, e2) -> 
    "Let rec " ^ var ^ "= " ^ 
    exp_to_concrete_string e1 ^ "in " ^ exp_to_concrete_string e2
  | Raise -> "Exception Raised"
  | Unassigned -> "Unassigned Expression"
  | App (e1, e2) -> "(" ^ exp_to_concrete_string e1 ^ ") (" 
                    ^ exp_to_concrete_string e2 ^ ") " ;;

(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with 
  | String x -> "String(" ^ x ^ ")"
  | Var x -> "Var(" ^ x ^ ")"
  | Num x -> "Num(" ^ string_of_int x ^ ")"
  | Float x -> "Float(" ^ string_of_float x ^ ")"
  | Bool x -> "Bool(" ^ (if x then "true" else "false") ^ ")"
  | Ref x -> "Ref(" ^ exp_to_abstract_string x ^ ")"
  | Unop (u, expr) -> let sign = 
                      match u with 
                      | Negate -> "Unop(Neg, " 
                      | Sqrt -> "Unop(Sqrt, " 
                      | Deref -> "Unop(Deref, " 
                      | Log -> "Unop(Log, " in
                      sign ^ exp_to_abstract_string expr ^ ")"
  | Binop (op, e1, e2) ->
      let sign =
      match op with
      | Plus -> "Plus, "
      | Minus -> "Minus, "
      | Times -> "Times, "
      | Divide -> "Divide, "
      | Equals -> "Equals, "
      | LessThan -> "LessThan, "
      | GreaterThan -> "GreaterThan, "
      | RefAlter -> "RefAlter, "
      in "Binop(" ^ sign ^ exp_to_abstract_string e1 ^ ", " ^ 
          exp_to_abstract_string e2 ^ ")"
  | Conditional (e1, e2, e3) ->
    "Conditional(" ^ exp_to_abstract_string e1 ^ ", " ^ 
    exp_to_abstract_string e2 ^ ", " ^ exp_to_abstract_string e3 ^ ")"
  | Fun (var, expr) -> 
    "Fun(" ^ var ^ ", " ^ exp_to_abstract_string expr ^ ")"
  | Let (var, e1, e2) -> 
    "Let(" ^ var ^ ", " ^ 
    exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  | Letrec (var, e1, e2) -> 
    "Letrec(" ^ var ^ ", " ^ 
    exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  | Raise -> "Excption Raised"
  | Unassigned -> "Unassigned Expression"
  | App (e1, e2) -> "App(" ^ exp_to_abstract_string e1 ^ ", " 
                    ^ exp_to_abstract_string e2 ^ ")" ;;
