module Evaluation where

import Function
import Reduction

apply :: Function -> Expression -> Expression
apply (Func param body) x = constReduce $ insertValue param x body

insertValue :: String -> Expression -> Expression -> Expression
insertValue p x f = emap applyExp
	where
		applyExp (Var s) = if s == p then x else Var s
		applyExp y       = y