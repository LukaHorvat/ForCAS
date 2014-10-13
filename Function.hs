module Function where

import Data.Maybe

type Number = Double

data BuiltInFunction = Abs | Sin | Cos | Sgn
    deriving (Show)

data Expression = Expression :*: Expression |
                  Expression :+: Expression |
                  Expression :/: Expression |
                  Expression :-: Expression |
                  F BuiltInFunction Expression |
                  C Number |
                  Var String
                  deriving (Show)

data Function = Func String Expression

infixl 7 :*:, :/:
infixl 6 :+:, :-:

swapOrMap :: (Expression -> Maybe Expression) -> Expression -> Expression
swapOrMap f e = fromMaybe (emap f e) (f e)

swapOrKeep :: (Expression -> Maybe Expression) -> Expression -> Expression
swapOrKeep f e = fromMaybe e (f e)

emap :: (Expression -> Maybe Expression) -> Expression -> Expression
emap f (x :*: y) = swapOrMap f x :*: swapOrMap f y
emap f (x :+: y) = swapOrMap f x :+: swapOrMap f y
emap f (x :/: y) = swapOrMap f x :/: swapOrMap f y
emap f (x :-: y) = swapOrMap f x :-: swapOrMap f y
emap f x         = swapOrKeep f x

instance Num Expression where
    (+) = (:+:)
    (*) = (:*:)
    abs = F Abs
    signum = F Sgn
    fromInteger = C . fromInteger
    (-) = (:-:)

instance Fractional Expression where
    fromRational = C . fromRational
    (/) = (:/:)