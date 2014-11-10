{-# LANGUAGE PatternSynonyms #-}
module Reduction where

import Data.Maybe
import Function
import Debug.Trace

pattern NumFrac n d = Number n :/: Number d

isConst :: Expression -> Bool
isConst (Number _)    = True
isConst (NumFrac _ _) = True
isConst _             = False

isOpString :: BinaryOp -> Expression -> Bool
isOpString op (Binary bop l r)
    | op == bop = isOp l || isOp r
    | otherwise = False
    where
        isOp (Binary op' _ _) = op == op'
        isOp _                = False
isOpString _ _ = False

collectOpString :: BinaryOp -> Expression -> [Expression]
collectOpString op e@(Binary bop l r)
    | op == bop = collectOpString op l ++ collectOpString op r
    | otherwise = [e]
collectOpString _ x = [x]

--Example:
-- op = Add
-- e =               +
--           +               +
--       +       4       *       3
--     2   3           3   2        
-- f acc (Leaf (Const x)) = Just $ Leaf $ Const $ acc + x
-- f _ _ = Nothing
-- start = Leaf $ Const 0
--
--Collects the sum into a list
--[2, 3, 4, 3 * 2, 3]
--Lets f eat all values it can and results in
--[12, 3 * 2]
--Reconstructs the tree
-- opwiseReduce op f e =
--         +
--     12     *
--          3   2
opwiseReduce :: BinaryOp -> (Expression -> Expression -> Maybe Expression) -> Expression -> Expression -> Expression
opwiseReduce op f start e = fromList op $ res : unprocessed
    where
        list = collectOpString op e
        folder :: (Expression, [Expression]) -> Expression -> (Expression, [Expression])
        folder (acc, remainder) expr = case f acc expr of
            Just newAcc -> (newAcc, remainder)
            Nothing -> (acc, expr : remainder)
        (res, unprocessed) = foldl folder (start, []) list

debug :: Show a => a -> a
debug x = trace (show x) x

try :: (Expression -> Maybe Expression) -> Expression -> Expression
try f e = fromMaybe e (f e)

symetric :: (Expression -> Maybe Expression) -> Expression -> Expression
symetric f e@(Binary op x y) = fromMaybe (fromMaybe e (f (Binary op y x))) (f e)
symetric _ e                 = e

--We do not cover cases that can't be reduced. Those are handled beforehand.
constantReduce :: Expression -> Expression
constantReduce (NumFrac n d)
    | g == d    = Number $ n `div` g
    | otherwise = NumFrac (n `div` g) (d `div` g)
    where g = gcd n d
constantReduce e@(_ :+: _) = symetric (tryReduce reduceAdd) e where
    reduceAdd (Number 0)    x          = Just x
    reduceAdd (Number x)    (Number y) = Just $ Number $ x + y
    reduceAdd (NumFrac n d) (Number x) = Just $ constantReduce $ NumFrac (d * x + n) d
    reduceAdd _             _          = Nothing
constantReduce e@(_ :*: _) = symetric (tryReduce reduceMult) e where
    reduceMult (Number 0)    _          = Just $ Number 0
    reduceMult (Number 1)    x          = Just x
    reduceMult (Number x)    (Number y) = Just $ Number $ x * y
    reduceMult (NumFrac n d) (Number x) = Just $ constantReduce $ NumFrac (n * x) d
    reduceMult _             _          = Nothing
constantReduce e@(_ :^: _) = try (tryReduce reducePow) e where
    reducePow _ (Number 0) = Just $ Number 1
    reducePow x (Number 1) = Just x
    reducePow (b :^: Number p1) (Number p2) = Just $ constantReduce $ b :^: Number (p1 * p2)
    reducePow b (Number p)
        | p < 0     = Nothing
        | otherwise = case b of
            (Number x)    -> Just $ Number $ x ^ p
            (NumFrac n d) -> Just $ NumFrac (n ^ p) (d ^ p)
            _             -> Nothing
    reducePow _ _   = Nothing
constantReduce e = e

tryReduce :: (Expression -> Expression -> Maybe Expression) -> Expression -> Maybe Expression
tryReduce f (Binary _ l r) = f l r
tryReduce _ _              = Nothing

constExprReduce :: Expression -> Expression
constExprReduce e@(Binary op _ _)
    | isOpString op e && op `elem` [Add, Mult] = opwiseReduce op f (neutral op) e
    | otherwise                                = constantReduce e
    where
        f acc new 
            | isConst new = Just $ constantReduce (Binary op acc new)
            | otherwise   = Nothing
constExprReduce e = e

intRoot :: Integer -> Integer -> Maybe Integer
intRoot p n
    | root ^ p == n = Just root
    | otherwise     = Nothing
    where root = round $ fromIntegral n ** ((1 :: Double) / fromIntegral p)