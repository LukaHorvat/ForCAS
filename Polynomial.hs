{-# LANGUAGE PatternSynonyms #-}
module Polynomial where

import Data.Ratio
import Function
import Reduction

binomialCoeff :: Integer -> Integer -> Integer
binomialCoeff n k = numerator $ product $ zipWith (%) [n, n - 1..] [k, k - 1..1]

binomialExpand :: Expression -> Expression
binomialExpand ((l :+: r) :^: Number n) = reduce constExprReduce $ sum $ zipWith term [n, n - 1..] [0..n] where
    term i j = Number (binomialCoeff n j) * (l :^: Number i) * (r :^: Number j) 
binomialExpand e = e