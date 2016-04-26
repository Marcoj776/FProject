module Arith where

type Stack = [Int]
type Env   = [(String, Int)]

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
eval (Add x y) bs 		=   eval x bs + eval y bs
eval (Gte x y) bs		=	if eval x bs >= eval y bs then 1 else 0
eval (Lte x y) bs 		=	if eval x bs <= eval y bs then 1 else 0
eval (Ite x y z) bs		=   if eval x bs /= 0 then eval y bs else eval z bs
eval (Lite x y z) bs 	=   if eval x bs /= 0 then eval y bs else eval z bs
eval (Let v x y) bs 	=   eval y ((v, eval x bs):bs) 
eval (Var v) bs		   	=   valueOf v bs 

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
comp' (Let v x y) cxt c	  =   comp' x cxt (LET (comp' y (v:cxt) (TEL c)))
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
{-
step1 :: String -> Expr -> Expr -> Context -> Code -> Memory -> [Memory]
step1 v x y cxt c (s,vs) =
  [ exec (comp' (Let v x y) cxt c) (s, vs),
    exec c ((eval (Let v x y) (zip cxt vs)):s, vs)  ]
	
step2 :: String -> Expr -> Expr -> Context -> Code -> Memory -> [Memory]
step2 v x y cxt c (s,vs) =
  [ exec c ((eval (Let v x y) (zip cxt vs)):s, vs),
    exec c (eval y ((v, eval x bs) :bs) :s, vs)  ]  
	where bs = (zip cxt vs)
	
step3 :: String -> Expr -> Expr -> Context -> Code -> Memory -> [Memory]
step3 v x y cxt c (s,vs) =
  [ exec c (eval y ((v, eval x bs):bs) :s, vs),
    exec c (eval y  ((v, eval x (zip cxt vs)) :(zip cxt vs)) :s, vs)  ]  
	where bs = (zip cxt vs) 
	
step4 :: String -> Expr -> Expr -> Context -> Code -> Memory -> [Memory]
step4 v x y cxt c (s,vs) =
  [ exec c (eval y  ((v, eval x (zip cxt vs)) :(zip cxt vs)) :s, vs),
    exec (TEL c) (eval y ((v, eval x bs) :bs) :s, eval x bs :vs)  ] 
	where bs = (zip cxt vs) 
	
step5 :: String -> Expr -> Expr -> Context -> Code -> Memory -> [Memory]
step5 v x y cxt c (s,vs) =
  [ exec (TEL c) (eval y ((v, eval x bs) :bs) :s, eval x bs :vs),
    exec (comp' y (v :cxt) (TEL c)) (s, eval x bs :vs)  ]  
	where bs = (zip cxt vs) 
	
step6 :: String -> Expr -> Expr -> Context -> Code -> Memory -> [Memory]
step6 v x y cxt c (s,vs) =
  [ exec (comp' y (v :cxt) (TEL c)) (s, eval x bs :vs),
    exec (LET (comp' y (v :cxt) (TEL c))) (eval x bs :s, vs)  ] 
	where bs = (zip cxt vs) 
	
step7 :: String -> Expr -> Expr -> Context -> Code -> Memory -> [Memory]
step7 v x y cxt c (s,vs) =
  [ exec (LET (comp' y (v :cxt) (TEL c))) (eval x bs :s, vs),
    exec (comp' x cxt (LET (comp' y (v :cxt) (TEL c)))) (s, vs)  ] 
	where bs = (zip cxt vs) 


one     = step1 "x" (Val 2) (Val 3) [] HALT ([],[])
two     = step2 "x" (Val 2) (Val 3) [] HALT ([],[])
three   = step3 "x" (Val 2) (Val 3) [] HALT ([],[])
four    = step4 "x" (Val 2) (Val 3) [] HALT ([],[])
five    = step5 "x" (Val 2) (Val 3) [] HALT ([],[])
six     = step6 "x" (Val 2) (Val 3) [] HALT ([],[])
seven   = step7 "x" (Val 2) (Val 3) [] HALT ([],[])
-}

let1 = comp (Let "a" (Val 1) (Var a))
{-
lite2 = exec(comp (Lite (Val 1) (Add (Val 2)(Val 3)) (Add (Val 4)(Val 5)))) []
lite3 =  eval (Ite (Val 1) (Add(Val 2)(Val 3)) (Add(Val 4)(Val 5)))

lite4 = comp (Lite (Val 0) (Add (Val 2)(Val 3)) (Add (Val 4)(Val 5)))
lite5 = exec(comp (Lite (Val 0) (Add (Val 2)(Val 3)) (Add (Val 4)(Val 5)))) []

lite6 =  eval (Ite (Val 0) (Add(Val 2)(Val 3)) (Add(Val 4)(Val 5)))
-}






