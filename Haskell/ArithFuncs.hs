module Arith where

type Stack = [Int]
type Env   = [(String, Int)]
type Fun   = [(String, Args)]
type Args  = [String]

data Expr = Val Int | 
			Add Expr Expr |
			Gte Expr Expr |
			Lte Expr Expr |
			Ite Expr Expr Expr |
			Lite Expr Expr Expr |
			LetVar String Expr Expr | Var String |
			LetFun String Args Expr | App String Args
			deriving (Eq, Show, Read)
			
			
eval              	   		::  Expr -> Env -> Fun -> Int
eval (Val n) bs fs			=   n
eval (Add x y) bs fs		=   eval x bs + eval y bs
eval (Gte x y) bs fs		=	if eval x bs >= eval y bs then 1 else 0
eval (Lte x y) bs fs		=	if eval x bs <= eval y bs then 1 else 0
eval (Ite x y z) bs	fs		=   if eval x bs /= 0 then eval y bs else eval z bs
eval (Lite x y z) bs fs		=   if eval x bs /= 0 then eval y bs else eval z bs
eval (LetVar v x y) bs fs	=   eval y ((v, eval x bs):bs) fs
eval (Var v) bs	fs	   		=   valueOf v bs fs
eval (LetFun f a y) bs fs	=	eval y bs ((f, [args]):fs)
									where args
eval (App f a) bs fs		= 	

--Auxillary function for retrieving value of a Var
valueOf :: String -> Env -> Int
valueOf s ([], _ ) = error "Binding out of scope?"
valueOf s ((v, n):bs, _ ) = if s == v then n else valueOf s bs
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
comp' (LetVar v x y) cxt c   =   comp' x cxt (LET (comp' y (v:cxt) (TEL c)))
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

--Enforces correctness of induction hypithesis
calc :: String -> Expr -> Expr -> Context -> Code -> Memory -> [Memory]
calc v x y cxt c (s,vs) =
  [
    exec (comp' (LetVar v x y) cxt c) (s, vs)
  , {-The induction hypothesis holds-}
    exec c ((eval (LetVar v x y) (zip cxt vs)):s, vs)
	-- (zip cxt vs) is of type Env
  ]
--













