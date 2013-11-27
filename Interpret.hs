module Interpret (interpret) where

import Lexer
import Parser
import Eval

import Utils

interpret :: String -> Result Expr
interpret str = lexer str >>= parse >>= eval
