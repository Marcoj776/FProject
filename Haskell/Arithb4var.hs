module Arith where

data Expr = Val Int | Add Expr Expr |
			Ite Expr Expr Expr |
			Lte Expr Expr Expr |
			Var String | Let Expr Expr Expr

eval            ::  Expr -> Int
eval (Val n)     =   n
eval (Add x y)   =   eval x + eval y
eval (Ite x y z) =   if eval x /= 0 then eval y else eval z
eval (Lte x y z) =   if eval x /= 0 then eval y else eval z
eval (Let (Var v) x y) bs =   eval y ((v, eval x bs):bs)
eval (Var v) bs     =   valueOf v bs

--Auxillary function for retrieving value of a Var
valueOf :: String -> Env -> Int
valueOf s [] = error "Binding out of scope?"
valueOf s ((v, n):bs) = if s == v then n else valueOf s bs
--

data Code             =   HALT | PUSH Int Code | ADD Code |
						  ITE Code | LTE Code Code	

comp 	              ::  Expr -> Code
comp e                =   comp' e HALT

comp'                 ::  Expr -> Code -> Code
comp' (Val n) c       =   PUSH n c
comp' (Add x y) c     =   comp' x (comp' y (ADD c))
comp' (Ite x y z) c   =   comp' z (comp' y (comp' x (ITE c)))
comp' (Lte x y z) c   =   comp' x (LTE (comp' y c) (comp' z c))


type Stack = [Int]

exec                  ::  Code -> Stack -> Stack
exec HALT s           =   s
exec (PUSH n c) s     =   exec c (n:s)
exec (ADD c) (m:n:s)  =   exec c ((n+m) : s)
exec (ITE c) (k:m:n:s)=   exec c ((if k /= 0 then m else n) : s)
exec (LTE ct ce) (k:s)=   exec (if k /= 0 then ct else ce) s


----TESTS-----
ift = exec (comp'(Ite (Val 1) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))) HALT) []
iff = exec (comp'(Ite (Val 0) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))) HALT) []
lift = exec (comp'(Ite (Val 1) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))) HALT) [] 
liff = exec (comp'(Ite (Val 0) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))) HALT) [] 
lliff = exec (comp'(Lte (Val 1) (Add (Val 1) (Add (Val 1) (Add (Val 1) (Val 0)))) (Add (Val 1) (Val 1))) HALT) []
-- [1][2][1][2] as expected



evvl = eval (Let (Var "x") (Let (Var "b") (Val 2) (Add (Var "b") (Val 2))) (Let (Var "y") (Var "x") (Add (Var "y") (Val 2)))) []






