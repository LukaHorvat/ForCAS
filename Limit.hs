module Limit where

import Function
import Evaluation

data Limit a = Finite a | PosInfinity | NegInfinity

limit :: Function a -> Limit a -> Maybe (Limit a)
limit (Func s e) l = case limitReduce $ insertValue s l (fmap Finite) e of
    Leaf (Const x) -> Just x
    _              -> Nothing

limitReduce :: Expression (Limit a) -> Expression (Limit a)
limitReduce = id