module Arith where

data Expr = Val Int | 
			Add Expr Expr |
			Ite Expr Expr Expr 

eval            ::  Expr -> Int
eval (Val n)     =   n
eval (Add x y)   =   eval x + eval y
eval (Ite x y z) =   if eval x /= 0 then eval y else eval z

data Code             =   HALT | PUSH Int Code | ADD Code |
						  ITE Code 

comp 	              ::  Expr -> Code
comp e                =   comp' e HALT

comp'                 ::  Expr -> Code -> Code
comp' (Val n) c       =   PUSH n c
comp' (Add x y) c     =   comp' x (comp' y (ADD c))
comp' (Ite x y z) c   =   comp' z (comp' y (comp' x (ITE c)))


type Stack = [Int]

exec                  ::  Code -> Stack -> Stack
exec HALT s           =   s
exec (PUSH n c) s     =   exec c (n:s)
exec (ADD c) (m:n:s)  =   exec c ((n+m) : s)
exec (ITE c) (k:m:n:s)=   exec c ((if k /= 0 then m else n) : s)


----TESTS-----
ift = exec (comp(Ite (Val 1) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))))
iff = exec (comp(Ite (Val 0) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))))
lift = exec (comp(Ite (Val 1) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))))
liff = exec (comp(Ite (Val 0) (Add (Val 1) (Val 0)) (Add (Val 1) (Val 1))))
-- [1][2][1][2] as expected







