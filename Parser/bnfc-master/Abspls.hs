

module Abspls where

-- Haskell module generated by the BNF converter




data Expr =
   Add Expr Expr
 | Mul Expr Expr
 | Ite Expr Expr Expr
 | Val Integer
  deriving (Eq,Ord,Show,Read)
