module Function where

import Data.Maybe

data Expression = Expression :*: Expression |
				  Expression :+: Expression |
				  Expression :/: Expression |
				  Expression :-: Expression |
				  C Float |
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
emap f x 		 = swapOrKeep f x