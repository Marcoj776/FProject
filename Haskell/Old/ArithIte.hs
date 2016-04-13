module ArithIte where

data Expr = Val Int | 
			Add Expr Expr |
			Ite Expr Expr Expr 
			deriving (Eq, Show, Read)

eval            ::  Expr -> Int
eval (Val n)     =   n
eval (Add x y)   =   eval x + eval y
eval (Ite x y z) =   if eval x /= 0 then eval y else eval z

data Code           =   HALT | PUSH Int Code | ADD Code |
						ITE Code 
						deriving (Eq, Show, Read)

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
if1 = comp(Ite (Val 1) (Add (Val 2) (Val 3)) (Add (Val 4) (Val 5)))
--PUSH 4 (PUSH 5 (ADD (PUSH 2 (PUSH 3 (ADD (PUSH 1 (ITE HALT)))))))
if2 = exec (comp(Ite (Val 1) (Add (Val 2) (Val 3)) (Add (Val 4) (Val 5)))) []
--[5]
if3 =  eval (Ite (Val 1) (Add(Val 2)(Val 3)) (Add(Val 4)(Val 5)))
--5

if4 = comp(Ite (Val 0) (Add (Val 2) (Val 3)) (Add (Val 4) (Val 5)))
--PUSH 4 (PUSH 5 (ADD (PUSH 2 (PUSH 3 (ADD (PUSH 0 (ITE HALT)))))))
if2 = exec (comp(Ite (Val 0) (Add (Val 2) (Val 3)) (Add (Val 4) (Val 5)))) []
--[5]
if3 =  eval (Ite (Val 0) (Add(Val 2)(Val 3)) (Add(Val 4)(Val 5)))
--5





