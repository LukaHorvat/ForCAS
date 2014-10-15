module Function where

type Number = Double

data BuiltInFunction = Abs | Sin | Cos | Sgn | Exp | Log | Asin | Acos | 
                       Sinh | Cosh | Asinh | Atanh | Acosh | Atan
                       deriving (Show)

data BinaryOp = Add | Sub | Mult | Div

data ExpLeaf a = Const a | Var String

data Expression a = Binary BinaryOp (Expression a) (Expression a) |
                    F BuiltInFunction (Expression a) |
                    Leaf (ExpLeaf a)

instance Show BinaryOp where
    show Add = "+"
    show Sub = "-"
    show Mult = "*"
    show Div = "/"

instance Show a => Show (ExpLeaf a) where
    show (Const x) = show x
    show (Var s) = s

instance Show a => Show (Expression a) where
    show (Binary op x y) = "(" ++ show x ++ " " ++ show op ++ " " ++ show y ++ ")"
    show (F f x) = show f ++ show x
    show (Leaf e) = show e

data Function a = Func String (Expression a)
    deriving (Show)

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

makeBinary :: BinaryOp -> a -> a -> Expression a
makeBinary op x y = Binary op (Leaf $ Const x) (Leaf $ Const y)

makeUnary :: BuiltInFunction -> a -> Expression a
makeUnary f x = F f (Leaf $ Const x)

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