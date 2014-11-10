{-# LANGUAGE PatternSynonyms #-}
module Function where

import Data.Ratio

type Number = Double

data BuiltInFunction = Abs | Sin | Cos | Sgn | Exp | Log | Asin | Acos | 
                       Sinh | Cosh | Asinh | Atanh | Acosh | Atan | Neg
                       deriving (Show, Eq)

data BinaryOp = Add | Mult | Pow
    deriving (Eq)

data Symbol = Infinity | Pi | E deriving Eq

data ExpLeaf = Const Integer | Var String | Special Symbol deriving Eq

data Expression = Binary BinaryOp Expression Expression |
                  F BuiltInFunction Expression |
                  Leaf ExpLeaf
                  deriving (Eq)

instance Show BinaryOp where
    show Add  = "+"
    show Mult = "*"
    show Pow  = "^"

instance Show Symbol where
    show Infinity = "inf"
    show Pi       = "pi"
    show E        = "e"

instance Show ExpLeaf where
    show (Const x) = show x
    show (Var s) = s
    show (Special s) = show s

instance Show Expression where
    show (x :*: (y :^: Number (-1))) = "(" ++ show x ++ " / " ++ show y ++ ")"
    show (Binary op x y) = "(" ++ show x ++ " " ++ show op ++ " " ++ show y ++ ")"
    show (F f x) = "(" ++ show f ++ " " ++ show x ++ ")"
    show (Leaf e) = show e

pattern Number x = Leaf (Const x)
pattern l :+: r  = Binary Add l r
pattern l :*: r  = Binary Mult l r
pattern l :/: r  = l :*: (r :^: (Number (-1)))
pattern l :-: r  = Binary Add l (F Neg r)
pattern l :^: r  = Binary Pow l r
pattern Minus x  = F Neg x

data Function = Func String Expression
    deriving (Show)

emap :: (ExpLeaf -> ExpLeaf) -> Expression -> Expression
emap f (Binary op x y) = Binary op (emap f x) (emap f y)
emap f (F fn e)        = F fn (emap f e)
emap f (Leaf x)        = Leaf $ f x

reduce :: (Expression -> Expression) -> Expression -> Expression
reduce f (Binary op x y) = f $ Binary op (reduce f x) (reduce f y)
reduce f (F fn e)        = f $ F fn (reduce f e)
reduce f x               = f x

var :: String -> Expression
var = Leaf . Var 

vx :: Expression
vx = var "x"

makeBinary :: BinaryOp -> Integer -> Integer -> Expression
makeBinary op x y = Binary op (Leaf $ Const x) (Leaf $ Const y)

makeUnary :: BuiltInFunction -> Integer -> Expression
makeUnary f x = F f (Leaf $ Const x)

fromList :: BinaryOp -> [Expression] -> Expression
fromList op = foldl1 (Binary op)

neutral :: BinaryOp -> Expression
neutral Add  = 0
neutral Mult = 1
neutral _    = error "Unsupported operation"

instance Num Expression where
    (+) = Binary Add
    (*) = Binary Mult
    abs = F Abs
    signum = F Sgn
    fromInteger = Leaf . Const . fromInteger
    negate (Number x) = Number (-x)
    negate x          = F Neg x

instance Fractional Expression where
    recip = flip (Binary Pow) (-1)
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance Floating Expression where
    pi = Leaf $ Special Pi
    exp ((F Log b) :*: p) = b :^: p 
    exp x = F Exp x
    log = F Log
    sin = F Sin
    cos = F Cos
    asin = F Asin
    atan = F Atan
    acos = F Acos
    sinh = F Sinh
    cosh = F Cosh
    asinh = F Asinh
    atanh = F Atanh
    acosh = F Acosh