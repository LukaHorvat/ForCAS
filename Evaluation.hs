module Evaluation where

import Function

apply :: Function -> Number -> Number
apply (Func param body) x = eval $ emap (insertValue param x) body

insertValue :: String -> Number -> Expression -> Maybe Expression
insertValue p x (Var s) = if s == p then Just $ C x else Nothing
insertValue _ _ _       = Nothing

eval :: Expression -> Number
eval (l :*: r) = eval l * eval r
eval (l :+: r) = eval l + eval r
eval (l :/: r) = eval l / eval r
eval (l :-: r) = eval l - eval r
eval (Var _)   = error "The expression has unset variables"
eval (C x)     = x