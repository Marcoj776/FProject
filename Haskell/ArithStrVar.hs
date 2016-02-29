module Arith where

type Stack = [Int]
type Env = [(String, Int)]

data Expr = Val Int | Add Expr Expr |
			Ite Expr Expr Expr |
			Lte Expr Expr Expr |
			Var String | Let String Expr Expr
			deriving (Eq, Show, Read)
--make first argument of Let a String, requires a lot of refactoring
			
eval              	   	 ::  Expr -> Env -> Int
eval (Val n) bs   	      =   n
eval (Add x y) bs	      =   eval x bs + eval y bs
eval (Ite x y z) bs 	  =   if eval x bs /= 0 then eval y bs else eval z bs
eval (Lte x y z) bs       =   if eval x bs /= 0 then eval y bs else eval z bs
eval (Let v x y) bs =   eval y ((v, eval x bs):bs)
eval (Var v) bs    		  =   valueOf v bs

--Auxillary function for retrieving value of a Var
valueOf :: String -> Env -> Int
valueOf s [] = error "Binding out of scope?"
valueOf s ((v, n):bs) = if s == v then n else valueOf s bs
--

data Code             =   HALT | PUSH Int Code | ADD Code |
						  ITE Code | LTE Code Code	|
						  LET Code | VAR Int Code | TEL Code
						  deriving (Eq, Show, Read)

comp 	              ::  Expr  -> Code
comp e                =   comp' e [] HALT

type Context = [String] -- this type might not be necessary

comp'                 ::  Expr -> Context -> Code -> Code

comp' (Val n) cxt c       =   PUSH n c
comp' (Add x y) cxt c     =   comp' x cxt (comp' y cxt (ADD c))
comp' (Ite x y z) cxt c   =   comp' z cxt (comp' y  cxt (comp' x cxt (ITE c)))
comp' (Lte x y z) cxt c   =   comp' x cxt (LTE (comp' y cxt c) (comp' z cxt c))
comp' (Let v x y) cxt c   =   comp' x cxt (LET (comp' y (v:cxt) (TEL c)))
comp' (Var v) cxt c       =   VAR (posOf v cxt) c

--Auxillary function for retrieving position of a Var in context stack
posOf :: String -> Context -> Int
posOf s [] = error "No position in []"
posOf s (v:vs) = if s == v then 0 else 1 + posOf s vs
--
type Memory = (Stack, Stack)
			

exec                         ::  Code -> Memory -> Memory 
exec HALT m                   =   m
exec (PUSH n c) (s, vs)       =   exec c ((n:s), vs)
exec (ADD c) ((m:n:s), vs)    =   exec c (((n+m) : s), vs)
exec (ITE c) ((k:m:n:s), vs)  =   exec c (((if k /= 0 then m else n) : s), vs)
exec (LTE ct ce) ((k:s), vs)  =   exec (if k /= 0 then ct else ce) (s, vs)
exec (LET c) ((n:s), vs)      =   exec c (s, n:vs)
exec (TEL c) (s, n:vs)        =   exec c (s, vs)
exec (VAR n c) (s, vs)        =   exec c (((vs!!n):s), vs) -- put value of n on top of s

----TESTS-----
ift = exec (comp
				(Ite (Val 1) 
					(Add (Val 1) (Val 0)) 
					(Add (Val 1) (Val 1)))) 
			([],[])
iff = exec (comp
				(Ite (Val 0) 
					(Add (Val 1) (Val 0)) 
					(Add (Val 1) (Val 1)))) 
			([],[])
lift = exec (comp
				(Ite (Val 1) 
					(Add (Val 1) (Val 0)) 
					(Add (Val 1) (Val 1)))) 
			([],[])
liff = exec (comp
				(Ite (Val 0) 
					(Add (Val 1) (Val 0)) 
					(Add (Val 1) (Val 1))))
			([],[])
-- [1][2][1][2] as expected
evv = eval (Let "x" (Val 1) 
				(Add (Var "x") (Val 2))) []
--[3]
evff = eval (Let "x" (Val 1) 
				(Add (Var "y") (Val 2))) []
-- *** Exception: Binding out of scope?
evvl = eval (Let "x"
				(Let "b" (Val 2) 
					(Add (Var "b") (Val 2)))
				(Let "y" (Var "x") 
					(Add (Var "y") (Val 2)))) []
--[6]
evffl = eval (Let "x" (Val 1) 
				(Let "y" (Var "x") 
					(Add (Var "Z") (Val 2)))) []
-- *** Exception: Binding out of scope?

--Enforces correctness of induction hypithesis
calc :: String -> Expr -> Expr -> Context -> Code -> Memory -> [Memory]
calc v x y cxt c (s,vs) =
--bs here being the stack of variable values
  [
    exec (comp' (Let v x y) cxt c) (s, vs)
  , {- reason 
	s is runtime Stack
	vs is variable values Stack-}
    exec c ((eval (Let v x y) (zip cxt vs)):s, vs)
	-- (zip cxt vs) is Env
  ]
--

ex = Add (Val 1) (Val 2)
wy = Add (Var "v") (Val 3)
onelet = calc "v" ex wy [] HALT ([],[])
letex = Let "x" (Val 3) (Add (Var "x") (Val 3))
twolet = calc "v" letex wy [] HALT ([],[])
--need to auto test this

posv = comp' (Var "v") ["v"] (comp' (Var "x") ["v", "y", "x"] HALT)
--the list here is cxt which is Context
tel = comp (Let "v" (Val 1) (Add (Val 2) (Var "v")))









