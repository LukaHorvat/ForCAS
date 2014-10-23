module Reduction where

import Function

data ReductionForm = Number Integer | Fraction Integer Integer | Complex Expression
    deriving (Show)

toForm :: Expression -> ReductionForm
toForm (Leaf (Const x)) = Number x
toForm (Binary Mult (Leaf (Const x)) (Binary Pow (Leaf (Const y)) (F Neg (Leaf (Const 1))) )) = Fraction x y
toForm x = Complex x

fromForm :: ReductionForm -> Expression
fromForm (Number x)     = fromIntegral x
fromForm (Fraction n d) = fromIntegral n / fromIntegral d
fromForm (Complex e)    = e

isConst :: Expression -> Bool
isConst e = case toForm e of
    Complex _ -> False
    _         -> True

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
opwiseReduce op f e start = fromList op $ res : unprocessed
    where
        list = collectOpString op e
        folder :: (Expression, [Expression]) -> Expression -> (Expression, [Expression])
        folder (acc, remainder) expr = case newRes of
            Just newAcc -> (newAcc, remainder)
            Nothing -> (acc, expr : remainder)
            where
                newRes = f acc e
        (res, unprocessed) = foldl folder (start, []) list

constantFracSimplify :: ReductionForm -> ReductionForm
constantFracSimplify (Fraction n d) 
    | g == d    = Number $ n `div` g
    | otherwise = Fraction (n `div` g) (d `div` g)
    where
        g = gcd n d
constantFracSimplify _              = error "Cannot simplify non-fractions"

--We do not cover cases that can't be reduced. Those are handled beforehand.
constantReduce :: BinaryOp -> ReductionForm -> ReductionForm -> ReductionForm
constantReduce Add  (Number x)     (Number y)     = Number $ x + y
constantReduce Add  (Fraction n d) (Number x)     = constantFracSimplify $ Fraction (d * x + n) d
constantReduce Add  x              y              = constantReduce Add y x --Addition is commutative
constantReduce Mult (Number x)     (Number y)     = Number $ x * y
constantReduce Mult (Fraction n d) (Number x)     = constantFracSimplify $ Fraction (n * x) d
constantReduce Mult x              y              = constantReduce Mult y x --Multiplication is commutative
constantReduce Pow  (Number x)     (Number y)     = Number $ x ^ y
constantReduce Pow  (Fraction n d) (Number x)     = Fraction (n ^ x) (d ^ x)
constantReduce Pow  (Number x)     (Fraction n d) = let y = x ^ n in case intRoot d y of
    Just root -> Number root
    Nothing   -> Complex $ fromIntegral y ** (1 / fromIntegral d)
constantReduce _    _              _              = error "Constant reduce given a case it cannot handle"

constExprReduce :: Expression -> Expression
constExprReduce e@(Binary op l r)
    | isOpString op e        = opwiseReduce op f 0 e
    | isConst l && isConst r = fromForm $ constantReduce op (toForm l) (toForm r)
    | otherwise              = e
    where
        f acc new 
            | isConst new = Just $ fromForm $ constantReduce op (toForm acc) (toForm new)
            | otherwise   = Nothing
constExprReduce e = e

intRoot :: Integer -> Integer -> Maybe Integer
intRoot p n
    | root ^ p == n = Just root
    | otherwise     = Nothing
    where root = round $ fromIntegral n ** ((1 :: Double) / fromIntegral p)