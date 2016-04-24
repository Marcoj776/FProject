module Arithmaybefun where

type Stack = [Int]
type Env = [(String, Int)]
--type Args = [String]
--type Funcs = [(String, Args)]

data Expr = Val Int | 
			Add Expr Expr |
			Gte Expr Expr |
			Lte Expr Expr |
			Ite Expr Expr Expr |
			Lite Expr Expr Expr |
			Let String Expr Expr | Var String |
			Def String String Expr Expr
			--Fun String Args
			deriving (Eq, Show, Read)
--make first argument of Let a String, requires a lot of refactoring
			
eval              	   	::  Expr -> Env -> Maybe Int
eval (Val n) bs 		=   Just n
eval (Add x y) bs		=   case eval x bs of 
							  Just n  -> case eval y bs of 
										   Just m  -> Just (n + m)
										   Nothing -> Nothing
							  Nothing -> Nothing
eval (Gte x y) bs 		=	case eval x bs of 
							  Just n  -> case eval y bs of 
										   Just m  -> Just (if n >= m then 1 else 0)
										   Nothing -> Nothing
							  Nothing -> Nothing
eval (Lte x y) bs 		=	case eval x bs of 
							  Just n  -> case eval y bs of 
										   Just m  -> Just (if n <= m then 1 else 0)
										   Nothing -> Nothing
							  Nothing -> Nothing
eval (Ite x y z) bs		=   case eval x bs of 
							  Just n  -> case eval y bs of 
										   Just m  -> case eval z bs of 
												Just i -> Just (if n /= 0 then m else i)
												Nothing -> Nothing												
										   Nothing -> Nothing
							  Nothing -> Nothing
eval (Lite x y z) bs 	=   case eval x bs of 
							  Just n  -> case eval y bs of 
										   Just m  -> case eval z bs of 
												Just i -> Just (if n /= 0 then m else i)
												Nothing -> Nothing												
										   Nothing -> Nothing
							  Nothing -> Nothing
eval (Let v x y) bs		=   case eval x bs of 
							  Just n  -> case eval y ((v, n):bs) of 
										   Just m  -> Just m
										   Nothing -> Nothing
							  Nothing -> Nothing
eval (Var v) bs		    =   Just (valueOf v bs)
--{-
eval (Def fun v op x) bs   = case eval x bs of
							  Just n  -> case eval op ((v, n):bs) of 
										   Just m  -> Just m
										   Nothing -> Nothing
							  Nothing -> Nothing 
---}
{-							  
eval (Def fun v op x) bs   = case eval (Let v x op) bs of
							  Just n  -> Just n
							  Nothing -> Nothing
-}
							  
deftest = eval (Def "plus" "n" (Add (Var "n") (Val 1)) (Val 1)) []

--Auxillary function for retrieving value of a Var
valueOf :: String -> Env -> Int
valueOf s [] = error "Binding out of scope?"
valueOf s ((v, n):bs) = if s == v then n else valueOf s bs
--

data Code             =   HALT | PUSH Int Code | ADD Code |
						  GTE Code | LTE Code |
						  ITE Code | LITE Code Code	|
						  LET Code | VAR Int Code | TEL Code |
						  DEF Code
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
ift = exec (comp(Ite (Val 1)(Add (Val 1) (Val 0))(Add (Val 1) (Val 1))))([],[])
iff = exec (comp(Ite (Val 0)(Add (Val 1) (Val 0))(Add (Val 1) (Val 1))))([],[])
lift = comp(Lite (Val 1) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1)))
liff = exec (comp(Lite (Val 0) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))))	([],[])
-- [1][2][1][2] as expected
evff = eval (Let "x" (Val 1)
				(Add (Var "y") (Val 2))) []
evvl = eval (Let "x" (Let "b" (Val 2) 
						(Add (Var "b") (Val 2)))
				(Let "y" (Var "x") 
					(Add (Var "y") (Val 2)))) []
evffl = eval (Let "x" (Val 1) 
				(Let "y" (Var "x") 
					(Add (Var "Z") (Val 2)))) []

--Enforces correctness of induction hypithesis
calc :: String -> Expr -> Expr -> Context -> Code -> Memory -> [Memory]
calc v x y cxt c (s,vs) =
  [
    exec (comp' (Let v x y) cxt c) (s, vs)
  , {-The induction hypothesis holds-}
    exec c (case eval (Let v x y) (zip cxt vs) of
				Just n -> (n:s, vs)
				Nothing -> error "Let evaluated to Nothing")
	-- (zip cxt vs) is of type Env
  ]
--

ex = Add (Val 1) (Val 2)
wy = Add (Var "v") (Val 3)
onelet = calc "v" ex wy [] HALT ([],[])
letex = Let "x" (Val 3) (Add (Var "x") (Val 3))
twolet = calc "v" letex wy [] HALT ([],[])
--need to auto test this

posv = comp' (Var "v") ["v"] (comp' (Var "x") ["v", "y", "x"] HALT)
-- ["v"] is cxt which is Context
tel = comp (Let "v" (Val 1) (Add (Val 2) (Var "v")))






