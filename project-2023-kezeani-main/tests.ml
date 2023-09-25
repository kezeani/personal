open Evaluation ;;
open Expr;;
open Env;;
open CS51Utils ;;
open Absbook ;;

 
let free_vars_tests () =
  unit_test ((free_vars (Var "x")) = (vars_of_list ["x"]))
  "free_vars var";;
  unit_test (free_vars (Num 1) = vars_of_list [])
  "free_vars num";;
  unit_test (free_vars (Float 1.) = vars_of_list [])
  "free_vars float";;
  unit_test (free_vars (Bool true) = vars_of_list [])
  "free_vars bool";;
  unit_test (free_vars Raise = vars_of_list [])
  "free_vars Raise";;
  unit_test (free_vars Unassigned = vars_of_list [])
  "free_vars Unassigned";;
  unit_test (free_vars (Unop (Negate, Var "x")) = vars_of_list ["x"])
  "free_vars Unop Var";;
  unit_test (free_vars (Unop (Negate, Num 3)) = vars_of_list [])
  "free_vars Unop Num";;
  unit_test (free_vars (Unop (Negate, Fun ("x", Binop(Plus, Var("x"), Var("y"))))) = vars_of_list ["y"])
  "free_vars Unop Function";;
  unit_test (free_vars (Binop (Plus, Var "x", Num 3)) = vars_of_list ["x"])
  "free_vars Binop One Var";;
  unit_test (free_vars (Binop (Times, Var "x", Var "x")) = vars_of_list ["x"])
  "free_vars Binop Same Var";;
  unit_test (free_vars (Binop (LessThan, Bool(true), Bool(false))) = vars_of_list [])
  "free_vars Binop Bools";;
  unit_test (free_vars (Fun ("x", Binop(Plus, Num(2), Num(2)))) = vars_of_list [])
  "free_vars Fun No Var";;
  unit_test (free_vars (Fun ("x", Binop(Plus, Var "x", Num 2))) = vars_of_list [])
  "free_vars Fun Same Var";;
  unit_test (free_vars (Fun ("x", Binop(Minus, Var "x", Var "y"))) = vars_of_list ["y"])
  "free_vars Fun Different Var";;
  unit_test (same_vars (free_vars (Fun ("x", Binop(Minus, Var "z", Var "y")))) (vars_of_list ["y"; "z"]))
  "free_vars Fun Multiple Frees";;
  unit_test (free_vars (Let ("x", Binop(Plus, Num(2), Num(2)), Var "x")) = vars_of_list [])
  "free_vars Let No Frees";;
  unit_test (free_vars (Let ("x", Binop(Plus, Var "x", Num 2), Var "x")) = vars_of_list ["x"])
  "free_vars Let Free in Assignment";;
  unit_test (same_vars (free_vars (Let ("x", Binop(Minus, Var "x", Num 2), Binop(Plus, Var "x", Var "y")))) (vars_of_list ["x"; "y"]))
  "free_vars Let Frees in Both Expressions";;
  unit_test (same_vars (free_vars (Let ("x", Binop(Minus, Var "x", Var "y"), Binop(Plus, Var "x", Var "y")))) (vars_of_list ["y"; "x"]))
  "free_vars Let Same Free in Both Expressions";;
  unit_test (free_vars (Letrec ("x", Binop(Plus, Num(2), Num(2)), Var "x")) = vars_of_list [])
  "free_vars Letrec No Frees";;
  unit_test (free_vars (Letrec ("x", Binop(Plus, Var "x", Num 2), Var "x")) = vars_of_list ["x"])
  "free_vars Letrec Free in Assignment";;
  unit_test (same_vars (free_vars (Letrec ("x", Binop(Minus, Var "x", Num 2), Binop(Plus, Var "x", Var "y")))) (vars_of_list ["x"; "y"]))
  "free_vars Letrec Frees in Both Expressions";;
  unit_test (same_vars (free_vars (Letrec ("x", Binop(Minus, Var "x", Var "y"), Binop(Plus, Var "x", Var "y")))) (vars_of_list ["y"; "x"]))
  "free_vars Letrec Same Free in Both Expressions";;
  unit_test (same_vars (free_vars (Conditional (Var "x", Binop(Plus, Num(2), Num(2)), Binop(Plus, Num 3, Var "y")))) (vars_of_list ["x"; "y"]))
  "free_vars Conditional 2 Frees";;
  unit_test (same_vars (free_vars (Conditional (Var "x", Binop(Plus, Var "y", Num 2), Var "z")))  (vars_of_list ["x"; "y"; "z"]))
  "free_vars Conditional Frees in All Three";;
  unit_test (same_vars (free_vars (Conditional (Bool true, Fun("x", Binop(Plus, Var "x", Num 2)), Fun("x", Num(3))))) (vars_of_list []))
  "free_vars Conditional Bound Functions";;
  unit_test (same_vars (free_vars (App (Fun("x", Binop(Plus, Var "x", Num 2)), Num 2))) (vars_of_list []))
  "free_vars Function No Frees";;
  unit_test (same_vars (free_vars (App (Fun("x", Binop(Plus, Var "x", Var "y")), Num 2))) (vars_of_list ["y"]))
  "free_vars Function Free in Function";;
  unit_test (same_vars (free_vars (App (Fun("x", Binop(Plus, Var "x", Num 2)), Binop(Plus, Num 2, Var "x")))) (vars_of_list ["x"]))
  "free_vars Function Free in Argument";;
  unit_test (same_vars (free_vars (App (Fun("x", Binop(Plus, Var "x", Var "y")), Binop(Plus, Num 2, Var "x")))) (vars_of_list ["x"; "y"]))
  "free_vars Function Free in Both";;

let subst_tests () =
  unit_test ((subst "x" (Num 2) (Num 3)) = (Num 3))
  "subst num";;
  unit_test ((subst "x" (Num 2) (Float 3.)) = (Float 3.))
  "subst float";;
  unit_test ((subst "x" (Num 2) (Bool true)) = (Bool true))
  "subst bool";; 
  unit_test ((subst "x" (Num 2) Raise) = (Raise))
  "subst Raise";; 
  unit_test ((subst "x" (Num 2) Unassigned) = (Unassigned))
  "subst Unassigned";;
  unit_test ((subst "x" (Num 2) (Var "x")) = (Num 2))
  "subst var relevant var";; 
  unit_test ((subst "x" (Num 2) (Var "y")) = (Var "y"))
  "subst var irrelevant var";;
  unit_test ((subst "x" (Num 2) (Unop (Negate, Var "x"))) = (Unop (Negate, Num 2)))
  "subst unop relevant var" ;; 
  unit_test ((subst "x" (Num 2) (Unop (Negate, Var "y"))) = (Unop (Negate, Var "y")))
  "subst unop irrelevant var";;
  unit_test ((subst "x" (Num 2) (Binop (Plus, Var "x", Num 2))) = (Binop (Plus, Num 2, Num 2)))
  "subst binop var in one" ;; 
  unit_test ((subst "x" (Num 2) (Binop (Plus, Var "x", Var "x"))) = (Binop (Plus, Num 2, Num 2)))
  "subst binop var in both" ;; 
  unit_test ((subst "x" (Num 2) (Binop (Plus, Var "x", Var "y"))) = (Binop (Plus, Num 2, Var "y")))
  "subst binop irrelevant var in one";;
  unit_test ((subst "x" (Num 2) (subst "y" (Num 3) (Binop (Plus, Var "x", Var "y")))) = (Binop (Plus, Num 2, Num 3)))
  "subst binop layered";;
  unit_test ((subst "x" (Num 2) (Binop (Plus, Var "x", Num 2))) = (Binop (Plus, Num 2, Num 2)))
  "subst binop var in one" ;; 
  unit_test ((subst "x" (Num 2) (Binop (Plus, Var "x", Var "x"))) = (Binop (Plus, Num 2, Num 2)))
  "subst binop var in both" ;; 
  unit_test ((subst "x" (Num 2) (Binop (Plus, Var "x", Var "y"))) = (Binop (Plus, Num 2, Var "y")))
  "subst binop irrelevant var in one";;
  unit_test ((subst "x" (Num 2) (subst "y" (Num 3) (Binop (Plus, Var "x", Var "y")))) = (Binop (Plus, Num 2, Num 3)))
  "subst binop layered";;
  unit_test ((subst "x" (Bool true) (Conditional (Var "x", Var "x", Bool false))) = (Conditional (Bool true, Bool true, Bool false)))
  "subst conditional";;
  unit_test ((subst "x" (Num 2) (subst "y" (Num 3) (subst "z" (Bool true) 
            (Conditional (Var "z", Var "x", Var "y"))))) = (Conditional (Bool true, Num 2, Num 3)))
  "subst conditional layered";;
  unit_test ((subst "x" (Num 2) (Fun ("x", Binop(Plus, Var "x", Num 5)))) = (Fun ("x", Binop(Plus, Var "x", Num 5))))
  "subst fun same bound var";;
  unit_test ((subst "y" (Num 2) (Fun ("x", Binop(Plus, Var "x", Var "y")))) = (Fun ("x", Binop(Plus, Var "x", Num 2))))
  "subst fun same unbound var";;
  unit_test ((subst "x" (Num 2) (Fun ("x", Binop(Plus, Var "x", Num 5)))) = (Fun ("x", Binop(Plus, Var "x", Num 5))))
  "subst fun same bound var";;
  unit_test ((subst "y" (Num 2) (Fun ("x", Binop(Plus, Var "x", Var "y")))) = (Fun ("x", Binop(Plus, Var "x", Num 2))))
  "subst fun var not in free variables";;
  unit_test ((subst "y" (Binop(Plus, Num 2, Var "x")) (Fun ("x", Binop(Plus, Var "x", Var "y")))) 
            = (Fun ("var1", Binop(Plus, Var "var1", (Binop(Plus, Num 2, Var "x"))))))
  "subst fun var in free variables";;
  unit_test ((subst "x" (Num 2) (Let ("x", Binop(Plus, Var "x", Num 5), Var "x"))) = (Let ("x", Binop(Plus, Num 2, Num 5), Var "x")))
  "subst let same var";;
  unit_test ((subst "y" (Num 2) (Let ("x", Binop(Plus, Var "y", Num 5), Binop(Plus, Var "x", Var "y")))) = 
                                (Let ("x", Binop(Plus, Num 2, Num 5), Binop(Plus, Var "x", Num 2))))
  "subst let var not in free variables";;
  unit_test ((subst "y" (Binop(Plus, Var "x", Num 2)) (Let ("x", Binop(Plus, Var "y", Num 5), Binop(Plus, Var "x", Var "y")))) = 
            (Let ("var2", Binop(Plus, Binop(Plus, Var "x", Num 2), Num 5), 
            Binop(Plus, Var "var2", Binop(Plus, Var "x", Num 2)))))
  "subst let var in free variables";;
  unit_test ((subst "x" (Num 2) (Letrec ("x", Fun("y", Binop(Plus, Var "y", Num 5)), App(Var "x", Num 10))))
                              = (Letrec ("x", Fun("y", Binop(Plus, Var "y", Num 5)), App(Var "x", Num 10))))
  "subst let rec same var";;
  unit_test ((subst "y" (Num 2) (Letrec ("x", Binop(Plus, Var "y", Num 5), Binop(Plus, Var "x", Var "y")))) = 
                                (Letrec ("x", Binop(Plus, Num 2, Num 5), Binop(Plus, Var "x", Num 2))))
  "subst let rec var not in free variables";;
  unit_test ((subst "y" (Binop(Plus, Var "x", Num 2)) (Letrec ("x", Binop(Plus, Var "y", Num 5), Binop(Plus, Var "x", Var "y")))) = 
            (Letrec ("var3", Binop(Plus, Binop(Plus, Var "x", Num 2), Num 5), 
            Binop(Plus, Var "var3", Binop(Plus, Var "x", Num 2)))))
  "subst let rec var in free variables";;
  unit_test ((subst "x" (Num 2) (App(Fun("y", Binop(Plus, Var "y", Var "x")), Binop(Plus, Num 10, Var "x"))))
  = (App(Fun("y", Binop(Plus, Var "y", Num 2)), Binop(Plus, Num 10, Num 2))))
  "subst app";; 

let eval_s_tests () =
  unit_test (eval_s (Num 5) (Env.empty ()) = Env.Val(Num 5))
  "eval_s num";;
  unit_test (eval_s (Float 5.) (Env.empty ()) = Env.Val(Float 5.))
  "eval_s float";;
  unit_test (eval_s (Bool true) (Env.empty ()) = Env.Val(Bool true))
  "eval_s bool";;
  unit_test (eval_s Raise (Env.empty ()) = Env.Val(Raise))
  "eval_s raise";;
  unit_test (eval_s Unassigned (Env.empty ()) = Env.Val(Unassigned))
  "eval_s unassigned";;
  unit_test (eval_s (Fun("x", Binop(Plus, Var "x", Num 2))) (Env.empty ()) 
            = Env.Val(Fun("x", Binop(Plus, Var "x", Num 2))))
  "eval_s function";;
  unit_test (try let _ = eval_s (Var "x") (Env.empty ()) in false with 
            | EvalError _ -> true 
            | _ -> false)
  "eval_s var";;
  unit_test (try let _ = eval_s (Unop (Negate, Bool true)) (Env.empty ()) in false with
             | EvalError _ -> true 
             | _ -> false)
  "eval_s invalid unop";;
  unit_test (eval_s (Unop (Negate, Num 5)) (Env.empty ()) = Env.Val(Num ~-5))
  "eval_s number unop";;
  unit_test (eval_s (Unop (Negate, Float 5.)) (Env.empty ()) = Env.Val(Float ~-.5.))
  "eval_s float unop";;
  unit_test (eval_s (Unop (Negate, Binop(Plus, Num 3, Num 2))) (Env.empty ()) = Env.Val(Num ~-5))
  "eval_s addition unop";;
  unit_test (eval_s (Unop (Negate, Let("x", Num 5, Var "x"))) (Env.empty ()) = Env.Val(Num ~-5))
  "eval_s let statement unop";;
  unit_test (eval_s (Binop (Plus, Num 2, Num 2)) (Env.empty ()) = Env.Val(Num 4))
  "eval_s plus";;
  unit_test (eval_s (Binop (Minus, Num 3, Num 2)) (Env.empty ()) = Env.Val(Num 1))
  "eval_s minus";;
  unit_test (eval_s (Binop (Times, Num 2, Num 2)) (Env.empty ()) = Env.Val(Num 4))
  "eval_s times";;
  unit_test (eval_s (Binop (Divide, Num 2, Num 2)) (Env.empty ()) = Env.Val(Num 1))
  "eval_s divide";;
  unit_test (try let _ = eval_s (Binop (Divide, Num 2, Bool false)) (Env.empty ()) in false with 
             | EvalError _ -> true 
             | _ -> false)
  "eval_s binop invalid";;
  unit_test (eval_s (Binop (Equals, Num 2, Num 2)) (Env.empty ()) = Env.Val(Bool true))
  "eval_s Num equals";;
  unit_test (eval_s (Binop (Equals, Bool false, Bool true)) (Env.empty ()) = Env.Val(Bool false))
  "eval_s Bool equals";;
  unit_test (eval_s (Binop (LessThan, Num 5, Num 2)) (Env.empty ()) = Env.Val(Bool false))
  "eval_s Num LessThan";;
  unit_test (eval_s (Binop (LessThan, Bool false, Bool true)) (Env.empty ()) = Env.Val(Bool true))
  "eval_s Bool LessThan";;
  unit_test (eval_s (Binop (GreaterThan, Num 5, Num 2)) (Env.empty ()) = Env.Val(Bool true))
  "eval_s Num GreaterThan";;
  unit_test (eval_s (Binop (GreaterThan, Bool false, Bool true)) (Env.empty ()) = Env.Val(Bool false))
  "eval_s Bool GreaterThan";;
  unit_test (try let _ = eval_s (Binop (Minus, Bool false, Bool true)) (Env.empty ()) in false with 
             | EvalError _ -> true 
             | _ -> false)
  "eval_s Bool Invalid Operator";;
  unit_test (eval_s (Conditional (Bool true, Num 5, Num 3)) (Env.empty ()) = Env.Val(Num 5))
  "eval_s Conditional True";;
  unit_test (eval_s (Conditional (Bool false, Num 5, Num 3)) (Env.empty ()) = Env.Val(Num 3))
  "eval_s Conditional False";;
  unit_test (try let _ = eval_s (Conditional (Num 3, Num 5, Num 3)) (Env.empty ()) in false with 
             | EvalError _ -> true 
             | _ -> false)
  "eval_s Conditional Invalid Condition";;
  unit_test (eval_s (Conditional (Binop(Equals, Num 2, Num 1), Num 5, Binop(Plus, Num 2, Num 1))) 
            (Env.empty ()) = Env.Val(Num 3))
  "eval_s Conditional Internal Computation";;
  unit_test (eval_s (Unop(Deref, Ref(Num 5))) (Env.empty ()) = Env.Val(Num 5))
  "eval_s Deref Num";;
  unit_test (try let _ = eval_s (Unop(Deref, Num 5)) (Env.empty ()) in false with 
              | EvalError _ -> true
              | _ -> false)
  "eval_s Deref Invalid";;
  unit_test (eval_s (Binop(RefAlter, Ref(Num 5), Num 6)) (Env.empty ()) = Env.Val(Ref(Num 6)))
  "eval_s Alter Ref";;
  unit_test (try let _ = eval_s (Binop(RefAlter, Ref(Num 5), Bool true)) (Env.empty ()) in false with 
            | EvalError _ -> true 
            | _ -> false)
  "eval_s Alter Ref Invalid";;
  unit_test (eval_s (Let ("x", Num 5, Binop(Plus, Var "x", Num 3))) (Env.empty ()) = Env.Val(Num 8))
  "eval_s Let Basic";;
  unit_test (eval_s (Let ("x", Num 5, Fun("y", Binop(Plus, Var "x", Var "y")))) (Env.empty ()) 
            = Env.Val(Fun("y", Binop(Plus, Num 5, Var "y"))))
  "eval_s Let Function substitution";;
  unit_test (try let _ = eval_s (Let ("x", Num 5, Binop(Plus, Var "x", Var "y"))) (Env.empty ()) in false with
             | EvalError _ -> true 
             | _ -> false)
  "eval_s Let Invalid Expression";;
  unit_test (eval_s (Letrec ("x", Fun("y", Conditional(Binop(Equals, Var "y", Num 0), Num 0, 
            Binop(Plus, Var "y", App(Var "x", Binop(Minus, Var "y", Num 1))))), 
            App(Var "x", Num 5))) (Env.empty ()) = Env.Val(Num 15))
  "eval_s Let Rec";;
  unit_test (eval_s (App (Fun("x", Binop(Plus, Var "x", Num 2)), Num 5)) (Env.empty ()) = Env.Val(Num 7))
  "eval_s App Basic";;
  unit_test (try let _ = eval_s (App (Fun("x", Binop(Plus, Var "x", Num 2)), Bool true)) (Env.empty ()) in false with 
             | EvalError _ -> true 
             | _ -> false)
  "eval_s App Invalid";;
  unit_test (eval_s (App (Fun("x", Binop(Plus, Var "x", Num 2)), Binop(Times, Num 3, Num 5))) (Env.empty ()) = Env.Val(Num 17))
  "eval_s App Internal Computational";;

  let eval_d_tests () =
    unit_test (eval_d (Num 5) (Env.empty ()) = Env.Val(Num 5))
    "eval_d num";;
    unit_test (eval_d (Float 5.) (Env.empty ()) = Env.Val(Float 5.))
    "eval_d float";;
    unit_test (eval_d (Bool true) (Env.empty ()) = Env.Val(Bool true))
    "eval_d bool";;
    unit_test (eval_d Raise (Env.empty ()) = Env.Val(Raise))
    "eval_d raise";;
    unit_test (eval_d Unassigned (Env.empty ()) = Env.Val(Unassigned))
    "eval_d unassigned";;
    unit_test (eval_d (Fun("x", Binop(Plus, Var "x", Num 2))) (Env.empty ()) 
              = Env.Val(Fun("x", Binop(Plus, Var "x", Num 2))))
    "eval_d function";;
    unit_test (try let _ = eval_d (Var "x") (Env.empty ()) in false with 
               | EvalError _ -> true 
               | _ -> false)
    "eval_d var empty env";;
    unit_test (eval_d (Var "x") (extend (Env.empty ()) "x" (ref(Env.Val(Num 2)))) = Env.Val(Num 2))
    "eval_d var assigned";;
    unit_test (try let _ = eval_d (Unop (Negate, Bool true)) (Env.empty ()) in false with
               | EvalError _ -> true 
               | _ -> false)
    "eval_d invalid unop";;
    unit_test (eval_d (Unop (Negate, Num 5)) (Env.empty ()) = Env.Val(Num ~-5))
    "eval_d number unop";;
    unit_test (eval_d (Unop (Negate, Float 5.)) (Env.empty ()) = Env.Val(Float ~-.5.))
    "eval_d float unop";;
    unit_test (eval_d (Unop (Negate, Binop(Plus, Num 3, Num 2))) (Env.empty ()) = Env.Val(Num ~-5))
    "eval_d addition unop";;
    unit_test (eval_d (Unop (Negate, Let("x", Num 5, Var "x"))) (Env.empty ()) = Env.Val(Num ~-5))
    "eval_d let statement unop";;
    unit_test (eval_d (Binop (Plus, Num 2, Num 2)) (Env.empty ()) = Env.Val(Num 4))
    "eval_d plus";;
    unit_test (eval_d (Binop (Minus, Num 3, Num 2)) (Env.empty ()) = Env.Val(Num 1))
    "eval_d minus";;
    unit_test (eval_d (Binop (Times, Num 2, Num 2)) (Env.empty ()) = Env.Val(Num 4))
    "eval_d times";;
    unit_test (eval_d (Binop (Divide, Num 2, Num 2)) (Env.empty ()) = Env.Val(Num 1))
    "eval_d divide";;
    unit_test (try let _ = eval_d (Binop (Divide, Num 2, Bool false)) (Env.empty ()) in false with 
               | EvalError _ -> true 
               | _ -> false)
    "eval_d binop invalid";;
    unit_test (eval_d (Binop (Equals, Num 2, Num 2)) (Env.empty ()) = Env.Val(Bool true))
    "eval_d Num equals";;
    unit_test (eval_d (Binop (Equals, Bool false, Bool true)) (Env.empty ()) = Env.Val(Bool false))
    "eval_d Bool equals";;
    unit_test (eval_d (Binop (LessThan, Num 5, Num 2)) (Env.empty ()) = Env.Val(Bool false))
    "eval_d Num LessThan";;
    unit_test (eval_d (Binop (LessThan, Bool false, Bool true)) (Env.empty ()) = Env.Val(Bool true))
    "eval_d Bool LessThan";;
    unit_test (eval_d (Binop (GreaterThan, Num 5, Num 2)) (Env.empty ()) = Env.Val(Bool true))
    "eval_d Num GreaterThan";;
    unit_test (eval_d (Binop (GreaterThan, Bool false, Bool true)) (Env.empty ()) = Env.Val(Bool false))
    "eval_d Bool GreaterThan";;
    unit_test (try let _ = eval_d (Binop (Minus, Bool false, Bool true)) (Env.empty ()) in false with 
               | EvalError _ -> true 
               | _ -> false)
    "eval_d Bool Invalid Operator";;
    unit_test (eval_d (Conditional (Bool true, Num 5, Num 3)) (Env.empty ()) = Env.Val(Num 5))
    "eval_d Conditional True";;
    unit_test (eval_d (Conditional (Bool false, Num 5, Num 3)) (Env.empty ()) = Env.Val(Num 3))
    "eval_d Conditional False";;
    unit_test (try let _ = eval_d (Conditional (Num 3, Num 5, Num 3)) (Env.empty ()) in false with 
               | EvalError _ -> true 
               | _ -> false)
    "eval_d Conditional Invalid Condition";;
    unit_test (eval_d (Conditional (Binop(Equals, Num 2, Num 1), Num 5, Binop(Plus, Num 2, Num 1))) 
              (Env.empty ()) = Env.Val(Num 3))
    "eval_d Conditional Internal Computation";;
    unit_test (eval_d (Unop(Deref, Ref(Num 5))) (Env.empty ()) = Env.Val(Num 5))
    "eval_d Deref Num";;
    unit_test (try let _ = eval_d (Unop(Deref, Num 5)) (Env.empty ()) in false with 
                | EvalError _ -> true
                | _ -> false)
    "eval_d Deref Invalid";;
    unit_test (eval_d (Binop(RefAlter, Ref(Num 5), Num 6)) (Env.empty ()) = Env.Val(Ref(Num 6)))
    "eval_d Alter Ref";;
    unit_test (try let _ = eval_d (Binop(RefAlter, Ref(Num 5), Bool true)) (Env.empty ()) in false with 
              | EvalError _ -> true 
              | _ -> false)
    "eval_d Alter Ref Invalid";;
    unit_test (eval_d (Let ("x", Num 5, Binop(Plus, Var "x", Num 3))) (Env.empty ()) = Env.Val(Num 8))
    "eval_d Let Basic";;
    unit_test (eval_d (Let ("x", Num 2, Let("f", Fun("y", Binop(Times, Var "x", Var "y")), Let("x", Num 1, App(Var "f", Num 21))))) (Env.empty ())
              = Env.Val(Num 21))
    "eval_d Let Function dynamic change";;
    unit_test (eval_d (Let ("x", Num 1, Let("f", Fun("y", Binop(Plus, Var "x", Var "y")), Let("x", Num 2, App(Var "f", Num 21))))) (Env.empty ())
              = Env.Val(Num 23))
    "eval_d Let Function dynamic change 2";;
    unit_test (try let _ = eval_d (Let ("x", Num 5, Binop(Plus, Var "x", Var "y"))) (Env.empty ()) in false with
               | EvalError _ -> true 
               | _ -> false)
    "eval_d Let Invalid Expression";;
    unit_test (eval_d (Letrec ("x", Fun("y", Conditional(Binop(Equals, Var "y", Num 0), Num 0, 
              Binop(Plus, Var "y", App(Var "x", Binop(Minus, Var "y", Num 1))))), 
              App(Var "x", Num 5))) (Env.empty ()) = Env.Val(Num 15))
    "eval_d Let Rec";;
    unit_test (eval_d (App (Fun("x", Binop(Plus, Var "x", Num 2)), Num 5)) (Env.empty ()) = Env.Val(Num 7))
    "eval_d App Basic";;
    unit_test (try let _ = eval_d (App (Fun("x", Binop(Plus, Var "x", Num 2)), Bool true)) (Env.empty ()) in false with 
               | EvalError _ -> true 
               | _ -> false)
    "eval_d App Invalid";;
    unit_test (eval_d (App (Fun("x", Binop(Plus, Var "x", Num 2)), Binop(Times, Num 3, Num 5))) (Env.empty ()) = Env.Val(Num 17))
    "eval_d App Internal Computational";;

  let eval_l_tests () =
    unit_test (eval_l (Num 5) (Env.empty ()) = Env.Val(Num 5))
    "eval_l num";;
    unit_test (eval_l (Float 5.) (Env.empty ()) = Env.Val(Float 5.))
    "eval_l float";;
    unit_test (eval_l (Bool true) (Env.empty ()) = Env.Val(Bool true))
    "eval_l bool";;
    unit_test (eval_l Raise (Env.empty ()) = Env.Val(Raise))
    "eval_l raise";;
    unit_test (eval_l Unassigned (Env.empty ()) = Env.Val(Unassigned))
    "eval_l unassigned";;
    unit_test (eval_l (Fun("x", Binop(Plus, Var "x", Num 2))) (Env.empty ()) 
              = Env.Closure((Fun("x", Binop(Plus, Var "x", Num 2))), Env.empty ()))
    "eval_l function";;
    unit_test (try let _ = eval_l (Var "x") (Env.empty ()) in false with 
                | EvalError _ -> true 
                | _ -> false)
    "eval_l var empty env";;
    unit_test (eval_l (Var "x") (extend (Env.empty ()) "x" (ref(Env.Val(Num 2)))) = Env.Val(Num 2))
    "eval_l var assigned";;
    unit_test (try let _ = eval_l (Unop (Negate, Bool true)) (Env.empty ()) in false with
                | EvalError _ -> true 
                | _ -> false)
    "eval_l invalid unop";;
    unit_test (eval_l (Unop (Negate, Num 5)) (Env.empty ()) = Env.Val(Num ~-5))
    "eval_l number unop";;
    unit_test (eval_l (Unop (Negate, Float 5.)) (Env.empty ()) = Env.Val(Float ~-.5.))
    "eval_l float unop";;
    unit_test (eval_l (Unop (Negate, Binop(Plus, Num 3, Num 2))) (Env.empty ()) = Env.Val(Num ~-5))
    "eval_l addition unop";;
    unit_test (eval_l (Unop (Negate, Let("x", Num 5, Var "x"))) (Env.empty ()) = Env.Val(Num ~-5))
    "eval_l let statement unop";;
    unit_test (eval_l (Binop (Plus, Num 2, Num 2)) (Env.empty ()) = Env.Val(Num 4))
    "eval_l plus";;
    unit_test (eval_l (Binop (Minus, Num 3, Num 2)) (Env.empty ()) = Env.Val(Num 1))
    "eval_l minus";;
    unit_test (eval_l (Binop (Times, Num 2, Num 2)) (Env.empty ()) = Env.Val(Num 4))
    "eval_l times";;
    unit_test (eval_l (Binop (Divide, Num 2, Num 2)) (Env.empty ()) = Env.Val(Num 1))
    "eval_l divide";;
    unit_test (try let _ = eval_l (Binop (Divide, Num 2, Bool false)) (Env.empty ()) in false with 
                | EvalError _ -> true 
                | _ -> false)
    "eval_l binop invalid";;
    unit_test (eval_l (Binop (Equals, Num 2, Num 2)) (Env.empty ()) = Env.Val(Bool true))
    "eval_l Num equals";;
    unit_test (eval_l (Binop (Equals, Bool false, Bool true)) (Env.empty ()) = Env.Val(Bool false))
    "eval_l Bool equals";;
    unit_test (eval_l (Binop (LessThan, Num 5, Num 2)) (Env.empty ()) = Env.Val(Bool false))
    "eval_l Num LessThan";;
    unit_test (eval_l (Binop (LessThan, Bool false, Bool true)) (Env.empty ()) = Env.Val(Bool true))
    "eval_l Bool LessThan";;
    unit_test (eval_l (Binop (GreaterThan, Num 5, Num 2)) (Env.empty ()) = Env.Val(Bool true))
    "eval_l Num GreaterThan";;
    unit_test (eval_l (Binop (GreaterThan, Bool false, Bool true)) (Env.empty ()) = Env.Val(Bool false))
    "eval_l Bool GreaterThan";;
    unit_test (try let _ = eval_l (Binop (Minus, Bool false, Bool true)) (Env.empty ()) in false with 
                | EvalError _ -> true 
                | _ -> false)
    "eval_l Bool Invalid Operator";;
    unit_test (eval_l (Conditional (Bool true, Num 5, Num 3)) (Env.empty ()) = Env.Val(Num 5))
    "eval_l Conditional True";;
    unit_test (eval_l (Conditional (Bool false, Num 5, Num 3)) (Env.empty ()) = Env.Val(Num 3))
    "eval_l Conditional False";;
    unit_test (try let _ = eval_l (Conditional (Num 3, Num 5, Num 3)) (Env.empty ()) in false with 
                | EvalError _ -> true 
                | _ -> false)
    "eval_l Conditional Invalid Condition";;
    unit_test (eval_l (Conditional (Binop(Equals, Num 2, Num 1), Num 5, Binop(Plus, Num 2, Num 1))) 
              (Env.empty ()) = Env.Val(Num 3))
    "eval_l Conditional Internal Computation";;
    unit_test (eval_l (Unop(Deref, Ref(Num 5))) (Env.empty ()) = Env.Val(Num 5))
    "eval_l Deref Num";;
    unit_test (try let _ = eval_l (Unop(Deref, Num 5)) (Env.empty ()) in false with 
                | EvalError _ -> true
                | _ -> false)
    "eval_l Deref Invalid";;
    unit_test (eval_l (Binop(RefAlter, Ref(Num 5), Num 6)) (Env.empty ()) = Env.Val(Ref(Num 6)))
    "eval_l Alter Ref";;
    unit_test (try let _ = eval_l (Binop(RefAlter, Ref(Num 5), Bool true)) (Env.empty ()) in false with 
            | EvalError _ -> true 
            | _ -> false)
    "eval_l Alter Ref Invalid";;
    unit_test (eval_l (Let ("x", Num 5, Binop(Plus, Var "x", Num 3))) (Env.empty ()) = Env.Val(Num 8))
    "eval_l Let Basic";;
    unit_test (eval_l (Let ("x", Num 2, Let("f", Fun("y", Binop(Times, Var "x", Var "y")), Let("x", Num 1, App(Var "f", Num 21))))) (Env.empty ())
              = Env.Val(Num 21))
    "eval_l Let Function dynamic change";;
    unit_test (try let _ = eval_l (Let ("x", Num 5, Binop(Plus, Var "x", Var "y"))) (Env.empty ()) in false with
                | EvalError _ -> true 
                | _ -> false)
    "eval_l Let Invalid Expression";;
    unit_test (eval_l (App (Fun("x", Binop(Plus, Var "x", Num 2)), Num 5)) (Env.empty ()) = Env.Val(Num 7))
    "eval_l App Basic";;
    unit_test (try let _ = eval_l (App (Fun("x", Binop(Plus, Var "x", Num 2)), Bool true)) (Env.empty ()) in false with 
                | EvalError _ -> true 
                | _ -> false)
    "eval_l App Invalid";;
    unit_test (eval_l (App (Fun("x", Binop(Plus, Var "x", Num 2)), Binop(Times, Num 3, Num 5))) (Env.empty ()) = Env.Val(Num 17))
    "eval_l App Internal Computational";;
let test_all () =
  free_vars_tests ();;
  subst_tests ();;
  eval_s_tests ();;
  eval_d_tests ();;
  eval_l_tests ();;


let _ = test_all () ;;