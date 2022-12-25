{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Lexer (module Lexer) where

import Text.Parsec.Language (haskell)

import qualified Text.Parsec.Token as Tok

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
