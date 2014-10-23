module Limit where

import Function
import Evaluation
import Control.Applicative ((<$>))

data Limit a = Finite a | PosInfinity | NegInfinity
    deriving (Show)

limit :: RealFloat a => Function a -> Limit a -> Maybe (Limit a)
limit (Func s e) l = case reduce limitReduce $ insertValue s l (fmap Finite) e of
    Leaf (Const x) -> Just x
    _              -> Nothing

negate :: Floating a => Limit a -> Limit a
negate PosInfinity = NegInfinity
negate NegInfinity = PosInfinity
negate (Finite a)  = Finite (-a)

binaryReduce :: RealFloat a => Limit a -> Limit a -> BinaryOp -> Maybe (Limit a)
{-
binaryReduce l r Add = case (l, r) of
    (PosInfinity, PosInfinity) -> Just PosInfinity
    (PosInfinity, NegInfinity) -> Nothing
    (PosInfinity, Finite x)    -> PosInfinity
    (NegInfinity, Finite x)    -> NegInfinity
    (NegInfinity, NegInfinity) -> NegInfinity
    (Finite x, Finite y)       -> Finite (x + y)
    _ -> binaryReduce r l Add
-}

binaryReduce PosInfinity PosInfinity op = case op of
    Add  -> Just PosInfinity
    Mult -> Just PosInfinity
    Pow  -> Just PosInfinity
    _    -> Nothing
binaryReduce PosInfinity NegInfinity op = case op of
    Sub  -> Just PosInfinity
    Mult -> Just NegInfinity
    _    -> Nothing
binaryReduce NegInfinity PosInfinity op = case op of
    Sub  -> Just NegInfinity
    Mult -> Just NegInfinity
    Pow  -> error "Negative infinity is out of bounds of the domain of the exponential function base"
    _    -> Nothing
binaryReduce NegInfinity NegInfinity op = case op of
    Add  -> Just NegInfinity
    Mult -> Just PosInfinity
    Pow  -> error "Negative infinity is out of bounds of the domain of the exponential function base"
    _    -> Nothing
binaryReduce (Finite x) PosInfinity op = case op of
    Add              -> Just PosInfinity
    Sub              -> Just NegInfinity
    Mult | x > 0     -> Just PosInfinity 
         | x == 0    -> Nothing
         | otherwise -> Just NegInfinity
    Div              -> Just $ Finite 0
    Pow  | x > 0     -> Just PosInfinity
         | x == 0    -> Nothing
         | otherwise -> error "Negative values are out of bounds of the domain of the exponential function base"
binaryReduce (Finite x) NegInfinity Pow
         | x > 0     -> Just $ Finite 0
         | x == 0    -> Nothing
         | otherwise -> error "Negative values are out of bounds of the domain of the exponential function base"
binaryReduce (Finite x) NegInfinity op = Limit.negate <$> binaryReduce (Finite x) PosInfinity op
binaryReduce PosInfinity (Finite x) op = case op of
    Add              -> Just PosInfinity
    Sub              -> Just PosInfinity
    Mult | x > 0     -> Just PosInfinity
         | x == 0    -> Just $ Finite 0 
         | otherwise -> Just NegInfinity
    Div  | x == 0    -> Nothing 
         | x > 0     -> Just PosInfinity 
         | otherwise -> Just NegInfinity
    Pow  | x > 0     -> Just PosInfinity
         | x == 0    -> Nothing
         | x < 0     -> Just $ Finite 0
binaryReduce NegInfinity (Finite _) 
binaryReduce NegInfinity (Finite x) op = Limit.negate <$> binaryReduce PosInfinity (Finite x) op
binaryReduce (Finite x) (Finite y) op = Just $ Finite $ eval $ makeBinary op x y

--data BuiltInFunction = Abs | Sin | Cos | Sgn | Exp | Log | Asin | Acos | 
--                       Sinh | Cosh | Asinh | Atanh | Acosh | Atan

unaryReduce :: Floating a => Limit a -> BuiltInFunction -> Maybe (Limit a)
unaryReduce PosInfinity Abs = Just PosInfinity
unaryReduce PosInfinity Sgn = Just $ Finite 1
unaryReduce PosInfinity Exp = Just PosInfinity
unaryReduce PosInfinity Log = Just PosInfinity
unaryReduce PosInfinity Asin = error "Infinity is out of bounds of the domain of asin"
unaryReduce PosInfinity Acos = error "Infinity is out of bounds of the domain of acos"
unaryReduce PosInfinity Sinh = Just PosInfinity
unaryReduce PosInfinity Cosh = Just PosInfinity
unaryReduce PosInfinity Asinh = Just PosInfinity
unaryReduce PosInfinity Acosh = Just PosInfinity
unaryReduce PosInfinity Atanh = error "Infinity is out of bounds of the domain of atanh"
unaryReduce PosInfinity Atan = Just $ Finite (pi / 2)
unaryReduce PosInfinity Sin = Nothing
unaryReduce PosInfinity Cos = Nothing
unaryReduce NegInfinity Abs = Just PosInfinity
unaryReduce NegInfinity Exp = Just $ Finite 0
unaryReduce NegInfinity Log = error "Negative infinity is out of bounds of the domain of log"
unaryReduce NegInfinity Cosh = Just PosInfinity
unaryReduce NegInfinity Acosh = error "Negative infinity is out of bounds of the domain of acosh"
unaryReduce NegInfinity x = Limit.negate <$> unaryReduce PosInfinity x
unaryReduce (Finite x) f = Just $ Finite $ eval $ makeUnary f x


limitReduce :: RealFloat a => Expression (Limit a) -> Expression (Limit a)
limitReduce e@(Binary op (Leaf (Const x)) (Leaf (Const y))) = case binaryReduce x y op of
    Just l  -> Leaf $ Const l
    Nothing -> e
limitReduce e@(F f (Leaf (Const x))) = case unaryReduce x f of
    Just l  -> Leaf $ Const l
    Nothing -> e
limitReduce x = x

doLim :: Expression a -> Expression (Limit a)
doLim = insertValue "x" PosInfinity (fmap Finite)