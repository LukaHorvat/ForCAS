module Function where

type Number = Double

data BuiltInFunction = Abs | Sin | Cos | Sgn | Exp | Log | Asin | Acos | 
                       Sinh | Cosh | Asinh | Atanh | Acosh | Atan
                       deriving (Show)

data BinaryOp = Add | Sub | Mult | Div
                deriving (Show)

data ExpLeaf a = Const a | Var String
                 deriving (Show)

data Expression a = Binary BinaryOp (Expression a) (Expression a) |
                    F BuiltInFunction (Expression a) |
                    Leaf (ExpLeaf a)
                    deriving (Show)

data Function a = Func String (Expression a)

emap :: (ExpLeaf a -> ExpLeaf b) -> Expression a -> Expression b
emap f (Binary op x y) = Binary op (emap f x) (emap f y)
emap f (F fn e)        = F fn (emap f e)
emap f (Leaf x)        = Leaf $ f x

reduce :: (Expression a -> Expression a) -> Expression a -> Expression a
reduce f (Binary op x y) = f $ Binary op (reduce f x) (reduce f y)
reduce f (F fn e)        = f $ F fn (reduce f e)
reduce f x               = f x

var :: String -> Expression a
var = Leaf . Var 

vx :: Expression a
vx = var "x"

instance Num a => Num (Expression a) where
    (+) = Binary Add
    (*) = Binary Mult
    abs = F Abs
    signum = F Sgn
    fromInteger = Leaf . Const . fromInteger
    (-) = Binary Sub

instance Fractional a => Fractional (Expression a) where
    fromRational = Leaf . Const . fromRational
    (/) = Binary Div

instance Floating a => Floating (Expression a) where
    pi = Leaf $ Const pi
    exp = F Exp
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

instance Functor ExpLeaf where
    fmap _ (Var s) = Var s
    fmap f (Const x) = Const $ f x