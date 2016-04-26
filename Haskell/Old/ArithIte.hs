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
if2 = exec (comp(Ite (Val 1) (Add (Val 2) (Val 3)) (Add (Val 4) (Val 5)))) []
if3 =  eval (Ite (Val 1) (Add(Val 2)(Val 3)) (Add(Val 4)(Val 5)))

if4 = comp(Ite (Val 0) (Add (Val 2) (Val 3)) (Add (Val 4) (Val 5)))
if5 = exec (comp(Ite (Val 0) (Add (Val 2) (Val 3)) (Add (Val 4) (Val 5)))) []
if6 =  eval (Ite (Val 0) (Add(Val 2)(Val 3)) (Add(Val 4)(Val 5)))


	step1 :: Expr -> Expr -> Expr -> Code -> Stack -> (Stack, Stack)
	step1 x y z c s = 
	  ( exec (comp' (Ite x y z) c) s,
		exec c (eval (Ite x y z) : s) )
	  
	step2 :: Expr -> Expr -> Expr -> Code -> Stack -> (Stack, Stack)
	step2 x y z c s = 
	  ( exec c (eval (Ite x y z) : s),
		exec c ((if eval x /= 0 then eval y else eval z) : s) )

	step3 :: Expr -> Expr -> Expr -> Code -> Stack -> (Stack, Stack)
	step3 x y z c s = 
	  ( exec c ((if eval x /= 0 then eval y else eval z) : s),
		exec (ITE c) (eval x : eval y : eval z : s) )

	step4 :: Expr -> Expr -> Expr -> Code -> Stack -> (Stack, Stack)
	step4 x y z c s = 
	  ( exec (ITE c) (eval x : eval y : eval z : s),
		exec (comp' x (ITE c)) (eval y : eval z : s) )

	step5 :: Expr -> Expr -> Expr -> Code -> Stack -> (Stack, Stack)
	step5 x y z c s = 
	  ( exec (comp' x (ITE c)) (eval y : eval z : s),
		exec (comp' y (comp' x (ITE c))) (eval z : s) )

	step6 :: Expr -> Expr -> Expr -> Code -> Stack -> (Stack, Stack)
	step6 x y z c s = 
	  ( exec (comp' y (comp' x (ITE c))) (eval z : s),
		exec (comp' z (comp' y (comp' x (ITE c)))) s )

	one   = step1 (Val 1) (Val 2) (Val 3) HALT []
	two   = step2 (Val 1) (Val 2) (Val 3) HALT []
	three = step3 (Val 1) (Val 2) (Val 3) HALT []
	four  = step4 (Val 1) (Val 2) (Val 3) HALT []
	five  = step5 (Val 1) (Val 2) (Val 3) HALT []
	six   = step6 (Val 1) (Val 2) (Val 3) HALT []









