module Evaluation where

import Function

apply :: Floating a => Function a -> a -> a
apply (Func param body) x = eval $ insertValue param x id body

insertValue :: String -> a -> (ExpLeaf b -> ExpLeaf a) -> Expression b -> Expression a
insertValue p x f e = emap applyExp e
	where
		applyExp (Var s) = if s == p then Const x else Var s
		applyExp y       = f y

eval :: Floating a => Expression a -> a
eval (Binary op l r) = case op of
	Add   -> x + y
	Sub   -> x - y
	Mult  -> x * y
	Div   -> x / y
	where
		x = eval l
		y = eval r
eval (F f e) = let use fn = fn $ eval e in case f of
	Sin   -> use sin
	Cos   -> use cos
	Abs   -> use abs
	Sgn   -> use signum
	Exp   -> use exp
	Log   -> use log
	Asin  -> use asin
	Acos  -> use acos
	Sinh  -> use sinh
	Cosh  -> use cosh
	Asinh -> use asinh
	Atanh -> use atanh
	Acosh -> use acosh
	Atan  -> use atan
eval (Leaf (Var _))   = error "The expression has unset variables"
eval (Leaf (Const x)) = x