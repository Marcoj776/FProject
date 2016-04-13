module TestArith where
import LazySmallCheck
import Arith

-- data Expr  = Val Int |
			 -- Add Expr Expr | 
			 -- Gte Expr Expr Expr | Lte Expr Expr Expr | 
			 -- Ite Expr Expr Expr | 
			 -- Lite Expr Expr Expr | 
			 -- Let String Expr Expr | Var String 

instance Serial Expr where
  series  =  const (drawnFrom [Val 1, Val 2]) \/
				cons2 Add \/
				cons2 Gte \/ cons2 Lte \/
				cons3 Ite \/ 
				cons3 Lite \/ 
				const (drawnFrom [Var "x", Var "y"]) \/
				cons3 Let
  
-- cons0 (Var "x") \/ cons0 (Var "y") ... cons2 (Let "x") \/ cons2 (Let "y")

wellScoped :: Expr -> Bool
wellScoped e  =  wellScopedIn [] e

wellScopedIn :: [String] -> Expr -> Bool
wellScopedIn _ (Val _)  =  True
wellScopedIn cxt (Add e1 e2)  =  wellScopedIn cxt e1 && wellScopedIn cxt e2
wellScopedIn cxt (Gte e1 e2) = wellScopedIn cxt e1 && wellScopedIn cxt e2
wellScopedIn cxt (Lte e1 e2) = wellScopedIn cxt e1 && wellScopedIn cxt e2
wellScopedIn cxt (Ite e1 e2 e3) = wellScopedIn cxt e1 && wellScopedIn cxt e2 && wellScopedIn cxt e3
wellScopedIn cxt (Lite e1 e2 e3) = wellScopedIn cxt e1 && wellScopedIn cxt e2 && wellScopedIn cxt e3
wellScopedIn cxt (Let e1 e2 e3) = wellScopedIn (e1:cxt) e2 && wellScopedIn (e1:cxt) e3
wellScopedIn cxt (Var v) = v `elem` cxt


prop_evalCompExec :: Expr -> Bool
prop_evalCompExec e  =  wellScoped e ==>
							case exec c ([],[]) of
							([x],[]) -> x == i
							_        -> False
	where
	c = comp e
	i = eval e []
--head fst(exec (comp e)([],[]))
--use more powerful statement to say that cxt of mem is empty []
--