module Usepls where

import Abspls
import Parpls
import ErrM

he (Ok x) = x
he (Bad s) = error s

hpl = he . pExpr . myLexer

count (Add a b) = 1 + count a + count b
count (Mul a b) = 1 + count a + count b
count (Ite a b c) = 1 + count a + count b + count c
count (Val _) = 1

chpl = count . hpl
