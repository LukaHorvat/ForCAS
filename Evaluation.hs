module Evaluation where

import Function

apply :: Function -> Float -> Float
apply (Func param body) x = eval $ emap (insertValue param x) body

insertValue :: String -> Float -> Expression -> Maybe Expression
insertValue p x (Var s) = if s == p then Just $ C x else Nothing
insertValue _ _ _       = Nothing

eval :: Expression -> Float
eval (l :*: r) = eval l * eval r
eval (l :+: r) = eval l + eval r
eval (l :/: r) = eval l / eval r
eval (l :-: r) = eval l - eval r
eval (Var _)   = error "The expression has unset variables"
eval (C x)     = x