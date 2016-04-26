module ArithAdd where

data Expr = Val Int | 
			Add Expr Expr 
			deriving (Eq, Show, Read)

eval            ::  Expr -> Int
eval (Val n)     =   n
eval (Add x y)   =   eval x + eval y

data Code             =   HALT | PUSH Int Code | ADD Code 
						  deriving (Eq, Show, Read)

comp 	              ::  Expr -> Code
comp e                =   comp' e HALT

comp'                 ::  Expr -> Code -> Code
comp' (Val n) c       =   PUSH n c
comp' (Add x y) c     =   comp' x (comp' y (ADD c))


type Stack = [Int]

exec                  ::  Code -> Stack -> Stack
exec HALT s           =   s
exec (PUSH n c) s     =   exec c (n:s)
exec (ADD c) (m:n:s)  =   exec c ((n+m) : s)


----TESTS-----
add1 = exec (comp(Add (Val 1) (Val 0))) []
add2 = exec (comp(Add (Add (Val 0) (Val 1)) (Val 2))) []
add3 = exec (comp(Add (Val 1) (Add (Val 0) (Val 2)))) []
add4 = exec (comp(Add (Add (Val 0) (Val 1)) (Add (Val 1) (Val 2)))) []

addcomp = comp(Add (Add (Val 0) (Val 1)) (Val 2))
execadd = exec (comp(Add (Add (Val 0) (Val 1)) (Val 2))) []

evaladd = eval (Add (Add (Val 0) (Val 1)) (Val 2))

	step1 :: Expr -> Expr  -> Code -> Stack -> (Stack, Stack)
	step1 x y c s = 
	  ( exec (comp' (Add x y) c) s,
		exec c (eval (Add x y) :s) )
	  
	step2 :: Expr -> Expr  -> Code -> Stack -> (Stack, Stack)
	step2 x y c s = 
	  ( exec c (eval (Add x y) :s),
		exec c ((eval x + eval y) :s) )  

	step3 :: Expr -> Expr  -> Code -> Stack -> (Stack, Stack)
	step3 x y c s = 
	  ( exec c ((eval x + eval y) :s),
		exec (ADD c) (eval y : eval x : s) )
		
	step4 :: Expr -> Expr  -> Code -> Stack -> (Stack, Stack)
	step4 x y c s = 
	  ( exec (ADD c) (eval y : eval x : s),
		exec (comp' y (comp' x (ADD c))) s )

	one = step1 (Val 1) (Val 2) HALT []
	two = step2 (Val 1) (Val 2) HALT []
	three = step3 (Val 1) (Val 2) HALT []
	four = step4 (Val 1) (Val 2) HALT []




