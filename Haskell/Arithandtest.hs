module Arithandtest where
import LazySmallCheck

type Stack = [Int]
type Env = [(String, Int)]

data Expr = Val Int | 
			Add Expr Expr |
			Gte Expr Expr |
			Lte Expr Expr |
			Ite Expr Expr Expr |
			Lite Expr Expr Expr |
			Let String Expr Expr | Var String
			deriving (Eq, Show, Read)
			
eval              	   	::  Expr -> Env -> Int
eval (Val n) bs 		=   n
eval (Add x y) bs		=   eval x bs + eval y bs
eval (Gte x y) bs 		=	if eval x bs >= eval y bs then 1 else 0
eval (Lte x y) bs 		=	if eval x bs <= eval y bs then 1 else 0
eval (Ite x y z) bs		=   if eval x bs /= 0 then eval y bs else eval z bs
eval (Lite x y z) bs 	=   if eval x bs /= 0 then eval y bs else eval z bs
eval (Let v x y) bs		=   eval y ((v, eval x bs):bs)
eval (Var v) bs		    =   valueOf v bs

(eval e ([],[])) == exec (comp e ([],[]))

--Auxillary function for retrieving value of a Var
valueOf :: String -> Env -> Int
valueOf s [] = error "Binding out of scope?"
valueOf s ((v, n):bs) = if s == v then n else valueOf s bs
--

data Code             =   HALT | PUSH Int Code | ADD Code |
						  GTE Code | LTE Code |
						  ITE Code | LITE Code Code	|
						  LET Code | VAR Int Code | TEL Code
						  deriving (Eq, Show, Read)

comp 	              ::  Expr  -> Code
comp e                =   comp' e [] HALT

type Context = [String]

comp'                 ::  Expr -> Context -> Code -> Code

comp' (Val n) cxt c       =   PUSH n c
comp' (Add x y) cxt c     =   comp' x cxt (comp' y cxt (ADD c))
--
comp' (Gte x y) cxt c     =   comp' y cxt (comp' x cxt (GTE c))
comp' (Lte x y) cxt c     =   comp' y cxt (comp' x cxt (LTE c))
--
comp' (Ite x y z) cxt c   =   comp' z cxt (comp' y  cxt (comp' x cxt (ITE c)))
comp' (Lite x y z) cxt c  =   comp' x cxt (LITE (comp' y cxt c) (comp' z cxt c))
--
comp' (Let v x y) cxt c   =   comp' x cxt (LET (comp' y (v:cxt) (TEL c)))
comp' (Var v) cxt c       =   VAR (posOf v cxt) c

--Auxillary function for retrieving position of a Var from the context stack
posOf :: String -> Context -> Int
posOf s [] = error "Var out of context"
posOf s (v:vs) = if s == v then 0 else 1 + posOf s vs
--

type Memory = (Stack, Stack)			

exec                         ::  Code -> Memory -> Memory 
exec  HALT m                   =   m
exec (PUSH n c) ( s, vs)       =   exec c ((n:s), vs)
exec (ADD c) 	((m:n:s), vs)  =   exec c (((n+m) : s), vs)
--
exec (GTE c)    ((m:n:s), vs)  =   exec c (((if m > n then 1 else 0) : s), vs)
exec (LTE c)    ((m:n:s), vs)  =   exec c (((if m < n then 1 else 0) : s), vs)
--
exec (ITE c)    ((k:m:n:s), vs)=   exec c (((if k /= 0 then m else n) : s), vs)
exec (LITE ct ce)((k:s), vs)   =   exec (if k /= 0 then ct else ce) (s, vs)
--
exec (LET c)    ((n:s), vs)    =   exec c (s, n:vs)
exec (TEL c) 	( s, n:vs)     =   exec c (s, vs)
exec (VAR n c)  ( s, vs)       =   exec c (((vs!!n):s), vs) -- put value of n on top of s
--

----TESTS-----
--Enforces correctness of induction hypithesis
calc :: String -> Expr -> Expr -> Context -> Code -> Memory -> [Memory]
calc v x y cxt c (s,vs) =
  [
    exec (comp' (Let v x y) cxt c) (s, vs)
  , {-The induction hypothesis holds-}
    exec c ((eval (Let v x y) (zip cxt vs)):s, vs)
	-- (zip cxt vs) is of type Env
  ]
--

instance Serial Expr where
  series  =  const (drawnFrom [Val 1, Val 2]) \/
				cons2 Add \/
				cons2 Gte \/ cons2 Lte \/
				cons3 Ite \/ 
				cons3 Lite \/ 
				const (drawnFrom [Var "x", Var "y"]) \/
				cons3 Let

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
wellScopedIn (v:cxt) (Var e) = if e == v then True else wellScopedIn cxt (Var e)

prop_evalCompExec :: Expr -> Bool
prop_evalCompExec e  =  wellScoped e ==> (eval e ([],[])) == exec (comp e ([],[]))










