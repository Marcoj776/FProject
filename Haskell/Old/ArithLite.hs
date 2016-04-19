module ArithLite where

data Expr = Val Int | 
			Add Expr Expr |
			Ite Expr Expr Expr |
			Lite Expr Expr Expr 
			deriving (Eq, Show, Read)

eval            ::  Expr -> Int
eval (Val n)     =   n
eval (Add x y)   =   eval x + eval y
eval (Ite x y z) =   if eval x /= 0 then eval y else eval z
eval (Lite x y z) =   if eval x /= 0 then eval y else eval z

data Code             =   HALT | PUSH Int Code | ADD Code |
						  ITE Code | LITE Code Code
						  deriving (Eq, Show, Read)

comp 	              ::  Expr -> Code
comp e                =   comp' e HALT

comp'                 ::  Expr -> Code -> Code
comp' (Val n) c       =   PUSH n c
comp' (Add x y) c     =   comp' x (comp' y (ADD c))
comp' (Ite x y z) c   =   comp' z (comp' y (comp' x (ITE c)))
comp' (Lite x y z) c   =   comp' x (LITE (comp' y c) (comp' z c))


type Stack = [Int]

exec                  ::  Code -> Stack -> Stack
exec HALT s           =   s
exec (PUSH n c) s     =   exec c (n:s)
exec (ADD c) (m:n:s)  =   exec c ((n+m) : s)
exec (ITE c) (k:m:n:s)=   exec c ((if k /= 0 then m else n) : s)
exec (LITE ct ce) (k:s)=   exec (if k /= 0 then ct else ce) s


----TESTS-----
lite1 = comp (Lite (Val 1) (Add (Val 2)(Val 3)) (Add (Val 4)(Val 5)))
lite2 = exec(comp (Lite (Val 1) (Add (Val 2)(Val 3)) (Add (Val 4)(Val 5)))) []
lite3 =  eval (Ite (Val 1) (Add(Val 2)(Val 3)) (Add(Val 4)(Val 5)))

lite4 = comp (Lite (Val 0) (Add (Val 2)(Val 3)) (Add (Val 4)(Val 5)))
lite5 = exec(comp (Lite (Val 0) (Add (Val 2)(Val 3)) (Add (Val 4)(Val 5)))) []

lite6 =  eval (Ite (Val 0) (Add(Val 2)(Val 3)) (Add(Val 4)(Val 5)))









