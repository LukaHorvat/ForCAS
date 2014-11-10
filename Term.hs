{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Term where

import Data.Function
import Data.List
import Function
import Reduction

data Term = Term { coefficient :: Expression
                 , variables :: [(String, Expression)]
                 } deriving (Show)

pattern V x = Leaf (Var x)

normalizeTerm :: Term -> Term
normalizeTerm (Term coef fs) = Term coef $ sortBy (compare `on` fst) $ map mult $ groupBy ((==) `on` fst) fs
    where
        mult list@((v, _):_) = (v, reduce constExprReduce $ fromList Add $ map snd list)
        mult _               = error "groupBy didn't work as it should"

isPower :: Expression -> Bool
isPower (V _ :^: _) = True
isPower (V _)       = True
isPower _           = False

power :: Expression -> (String, Expression)
power (V s :^: e) = (s, e)
power (V s)       = (s, 1)
power _           = error "Not a power"

isTerm :: Expression -> Bool
isTerm (Minus (isTerm -> True)) = True
isTerm e = length consts == 1 && all isPower powers where
    prod = collectOpString Mult e
    (consts, powers) = partition isConst prod

fromExpression :: Expression -> Term
fromExpression (Minus e@(isTerm -> True)) = Term (-coef) vars where
    Term coef vars = fromExpression e
fromExpression e@(isTerm -> True)       = normalizeTerm $ Term factor $ map power powers where
    prod = collectOpString Mult e
    (factor:_, powers) = partition isConst prod
fromExpression _                  = error "Not a term"

toExpression :: Term -> Expression
toExpression (Term coef vars) = coef * varsExp where
    varsExp = fromList Mult $ map varExp vars
    varExp (str, p) = V str :^: p

basicSymbolicReduce :: Expression -> Expression
basicSymbolicReduce e
    | all isTerm sumList = reduce constExprReduce $ fromList Add $ map termExp $ groupBy ((==) `on` variables) terms
    | otherwise          = e
    where
        sumList = collectOpString Add e
        terms = map fromExpression sumList
        termExp termList@(Term _ vars:_) = toExpression $ Term coef vars where
            coef = reduce constExprReduce $ sum $ map coefficient termList
        termExp _ = error "groupBy didn't work as it should"

