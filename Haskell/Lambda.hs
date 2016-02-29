import LazySmallCheck
import Data.List ((\\), union)

data Exp  =  Var Char | Lam Char Exp | App Exp Exp
             deriving (Show)

instance Serial Exp where
  series  =  {- cons1 Var -} const (drawnFrom [Var 'x', Var 'y']) \/ cons2 Lam \/ cons2 App

-- possible variation instead of cons1 Var
-- const (drawnFrom [Var 'x', Var 'y'])

-- list the free variables in an expression
free :: Exp -> [Char]
free (Var v)      =  [v]
free (Lam v e)    =  free e \\ [v]
free (App e1 e2)  =  free e1 `union` free e2

-- list all the variables occurring in an expression
-- both "declarations" in lambdas, and uses
vars :: Exp -> [Char]
vars (Var v)      =  [v]
vars (Lam v e)    =  [v] `union` vars e
vars (App e1 e2)  =  vars e1 `union` vars e2

prop_freeSubVars :: Exp -> Bool
prop_freeSubVars e  =  wff e ==> (free e `subset` vars e)
  where
  xs `subset` ys  =  null (xs \\ ys)

wff :: Exp -> Bool
wff (Var v) = True
wff (Lam v e) = True
