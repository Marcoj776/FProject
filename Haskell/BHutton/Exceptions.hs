module Exceptions where

import Prelude hiding (fail)

data Expr = Val Int | Add Expr Expr | Throw | Catch Expr Expr
			deriving Show

eval             :: Expr -> Maybe Int
eval (Val n)     =  Just n
eval (Add x y)   =  case eval x of 
                      Just n  -> case eval y of 
                                   Just m  -> Just (n + m)
                                   Nothing -> Nothing
                      Nothing -> Nothing
eval Throw       =  Nothing
eval (Catch x h) =  case eval x of
                       Just n  -> Just n
                       Nothing -> eval h

one, two, three, four :: Expr
--Explicitly defines type, more helpful error msgs
one = Add (Val 1) Throw
--Nothing
two = Catch one (Val 0)
--Just 0
three = Throw
--trying to raise an exception
four = Add (Val 0) Throw
--Nothing

data Code = HALT | PUSH Int Code | ADD Code |
            FAIL | MARK Code Code | UNMARK Code
			deriving Show


comp		    :: Expr -> Code
comp e		    =  comp' e HALT

comp'		    :: Expr -> Code -> Code 
comp' (Val n) c     =  PUSH n c
comp' (Add x y) c   =  comp' x (comp' y (ADD c))
comp' Throw c       =  FAIL
comp' (Catch x h) c =  MARK (comp' h c) (comp' x (UNMARK c))

five:: Expr
five = Add (Val 3) (Val 4)
--comp five
--PUSH 3 (PUSH 4 (ADD HALT))
--eval five
--Just 7
--explore QuickCheck in syac practicals
gslgnsel
--use it to find exceptions
six = comp' (Add (Val 1) (Val 2)) (PUSH 3 HALT)

type Stack = [Elem]
data Elem  = VAL Int | HAN Code
			deriving Show

exec		                    :: Code -> Stack -> Stack
exec HALT s			    =  s
exec (PUSH n c) s                   =  exec c (VAL n : s)
exec (ADD c) (VAL m : VAL n : s)    =  exec c (VAL (n+m) : s)
exec FAIL s                         =  fail s
exec (MARK c' c) s                  =  exec c (HAN c' : s)
exec (UNMARK c) (VAL n : HAN _ : s) =  exec c (VAL n : s)

fail		     		    :: Stack -> Stack
fail []                             =  []
fail (VAL n : s)     		    =  fail s
fail (HAN c : s)      		    =  exec c s
