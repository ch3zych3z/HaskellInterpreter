module Lexer where

import Ast

import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskell)
import Text.Parsec.Prim (many)

import qualified Text.Parsec.Token as Tok

-- haskell :: Tok.TokenParser ()
-- haskell = Tok.makeTokenParser style
--   where
--     ops = [ "+", "*", "-", "/"
--           , "<", ">", ">=", "<="
--           , "==" ]
--     names = ["True", "False", "Int", "Bool", "->", "\\", "let", "in", "="]
--     style = emptyDef {
--                Tok.commentLine = "--"
--              , Tok.commentStart = "{-"
--              , Tok.commentEnd = "-}"
--              , Tok.caseSensitive = True
--              , Tok.reservedOpNames = ops
--              , Tok.reservedNames = names
--              }

integer    = Tok.natural haskell
parens     = Tok.parens haskell
braces     = Tok.braces haskell
brackets   = Tok.brackets haskell
commaSep   = Tok.commaSep haskell
semiSep    = Tok.semiSep haskell
identifier = Tok.identifier haskell
whitespace = Tok.whiteSpace haskell
reserved   = Tok.reserved haskell
reservedOp = Tok.reservedOp haskell
semi       = Tok.semi haskell
sym        = Tok.symbol haskell

