module Arith where

type Stack = [Int]
type Env = [(String, Int)]

data Expr = Val Int | Add Expr Expr |
			Ite Expr Expr Expr |
			Lte Expr Expr Expr |
			Var String | Let Expr Expr Expr

eval               ::  Expr -> Env -> Int
eval (Val n) bs     =   n
eval (Add x y) bs   =   eval x bs + eval y bs
eval (Ite x y z) bs =   if eval x bs /= 0 then eval y bs else eval z bs
eval (Lte x y z) bs =   if eval x bs /= 0 then eval y bs else eval z bs
eval (Let (Var v) x y) bs =   eval y ((v, eval x bs):bs)
eval (Var v) bs     =   valueOf v bs

--Auxillary function for retrieving value of a Var
valueOf :: String -> Env -> Int
valueOf s [] = error "Binding out of scope?"
valueOf s ((v, n):bs) = if s == v then n else valueOf s bs
--

data Code             =   HALT | PUSH Int Code | ADD Code |
						  ITE Code | LTE Code Code	|
						  LET Code | VAR Int Code

comp 	              ::  Expr  -> Code
comp e                =   comp' e HALT

comp'                 ::  Expr -> Code -> Code
comp' (Val n) c       =   PUSH n c
comp' (Add x y) c     =   comp' x (comp' y (ADD c))
comp' (Ite x y z) c   =   comp' z (comp' y  (comp' x (ITE c)))
comp' (Lte x y z) c   =   comp' x (LTE (comp' y  c) (comp' z  c))
--comp' (Let v x y) cxt c   =   comp' x (v:cxt) c
--comp' (Var v) cxt c       =   VAR (posOf v cxt) c

--Auxillary function for retrieving position of a Var in context stack
posOf :: String -> [String] -> Int
posOf s [] = error "No position in []"
posOf s (v:vs) = if s == v then 0 else 1 + posOf s vs
--

exec                  ::  Code -> Stack -> Stack 
exec HALT s             =   s
exec (PUSH n c) s       =   exec c (n:s) 
exec (ADD c) (m:n:s)    =   exec c ((n+m) : s) 
exec (ITE c) (k:m:n:s)  =   exec c ((if k /= 0 then m else n) : s)
exec (LTE ct ce) (k:s)  =   exec (if k /= 0 then ct else ce) s
--exec (LET c) (m:n:s)    =   exec
exec (VAR n c) s        =   exec c (s!!n:s)


----TESTS-----
ift = exec (comp'(Ite (Val 1) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))) HALT) []
iff = exec (comp'(Ite (Val 0) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))) HALT) []
lift = exec (comp'(Ite (Val 1) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))) HALT) [] 
liff = exec (comp'(Ite (Val 0) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))) HALT) []
-- [1][2][1][2] as expected
evv = eval (Let (Var "x") (Val 1) (Add (Var "x") (Val 2))) []
evff = eval (Let (Var "x") (Val 1) (Add (Var "y") (Val 2))) []
--[3]
-- *** Exception: Binding out of scope?
evvl = eval (Let (Var "x") (Let (Var "b") (Val 2) (Add (Var "b") (Val 2))) (Let (Var "y") (Var "x") (Add (Var "y") (Val 2)))) []
evffl = eval (Let (Var "x") (Val 1) (Let (Var "y") (Var "x") (Add (Var "Z") (Val 2)))) []
--[6]
-- *** Exception: Binding out of scope?
{-}
type Memory = (Stack, Stack)

calc :: String -> Expr -> Expr -> [String] -> Code -> Memory -> [Memory]
calc v x y vs c (s,bs) =
  [
    exec (comp' (Let v x y) vs c) s bs
  , {- reason -}
    exec c (eval (Let v x y) vs : s) bs
  ]
-}

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
