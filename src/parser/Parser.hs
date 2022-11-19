{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parser (parseExpr, parseProgram) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad.Except
import Data.List (sort)
import qualified Data.Map as Map (empty)
import Data.Functor ( ($>) )

import qualified Text.Parsec.Expr as Ex

import Ast
import Lexer
import Pattern (collectIdents)

allDifferent :: Ord a => [a] -> Bool
allDifferent = pairwiseDifferent . sort

pairwiseDifferent :: Eq a => [a] -> Bool
pairwiseDifferent xs = and $ zipWith (/=) xs (drop 1 xs)

unfoldAbs :: [HPattern] -> HExpr -> HExpr
unfoldAbs [i]      e = HEAbs i e
unfoldAbs (i : is) e = HEAbs i $ unfoldAbs is e
unfoldAbs []       _ = error "unfolding empty list"

unique :: [HPattern] -> Parser ()
unique pats = do
  let idents = collectIdents pats
  unless (allDifferent idents) $ fail "conflicting definitions"

program :: Parser HProgram
program = do
  void spaces
  bs <- semiSep $ try binding
  case reverse bs of
    (mn : bsr) ->
      case mn of
        Bind "main" (HEApp (HEVar _ "print") e) -> return $ Binds (reverse bsr) e
        _                                     -> fail "main function is not detected or it is not last defined function"
    _          -> fail "at least one function (main) should be defined"

expr :: Parser HExpr
expr = Ex.buildExpressionParser table term

term :: Parser HExpr
term = try letIn
   <|> try caseOf
   <|> try application
   <|> try abstraction
   <|> try ifelse
   <|> operand

operand :: Parser HExpr
operand = parens (try expr) <|> try value <|> try var

table = [ {-[ unIntOp "-" UnMinus,
            unBoolOp "!" Not]
        ,-} [ binIntOp "*" Mul,
            binIntOp "/" Div]
        , [ binIntOp "+" Add,
            binIntOp "-" Sub]
        , [ compOp "==" Eqls,
            compOp ">" Gr,
            compOp "<" Le,
            compOp ">=" Greq,
            compOp "<=" Leq
            --compOp "!=" Neq
            ]
        ]

binary name fun = Ex.Infix ( do { reservedOp name; return fun } )

binIntOp s op = binary s (`HEBinOp` op) Ex.AssocLeft

compOp s op = binary s (`HEBinOp` op) Ex.AssocNone

caseOf :: Parser HExpr
caseOf = do
  reserved "case"
  e <- expr
  reserved "of"
  ms <- braces $ semiSep matching
  return $ HECase e ms

matching :: Parser Matching
matching = do
  p <- patternP
  reservedOp "->"
  e <- expr
  return $ p :->: e

letIn :: Parser HExpr
letIn = do
  reserved "let"
  bs <- bindingList
  reserved "in"
  HELet . Binds bs <$> expr

bindingList :: Parser [Binding]
bindingList = braces $ semiSep binding

binding :: Parser Binding
binding = do
  i <- identifier
  args <- many patternP
  unique (HPIdent i : args)
  reservedOp "="
  e <- expr
  return $ case args of
    [] -> Bind i e
    _  -> Bind i $ unfoldAbs args e



ifelse :: Parser HExpr
ifelse = do
  reserved "if"
  cond <- expr
  reserved "then"
  e1   <- expr
  reserved "else"
  HEIf cond e1 <$> expr

abstraction :: Parser HExpr
abstraction = do
  reservedOp "\\"
  idents <- many1 patternP
  unique idents
  reservedOp "->"
  unfoldAbs idents <$> expr

application :: Parser HExpr
application = do
  i          <- var
  app : apps <- sepBy1 operand spaces
  return $ foldl HEApp (HEApp i app) apps

-- patterns
patternP :: Parser HPattern
patternP =
      try (braces patternP)
  <|> try labelPat
  <|> try valuePat
  <|> try identPat
  <|> try wildcardPat

wildcardPat :: Parser HPattern
wildcardPat = reserved "_" $> HPWildcard

labelPat :: Parser HPattern
labelPat = do
  i <- identifier
  reservedOp "@"
  HPLabel i <$> patternP

identPat :: Parser HPattern
identPat = HPIdent <$> identifier

valuePat :: Parser HPattern
valuePat = let
  vpint :: Parser HValuePat
  vpint = toVPInt <$> integer
    where
      toVPInt x = HVPInt $ fromInteger x
  vpbool :: Parser HValuePat
  vpbool = (toVP True <$> try (reserved "True")) <|> (toVP False <$> try (reserved "False"))
    where
      toVP v = const $ HVPBool v
  in HPVal <$> (vpint <|> vpbool)

-- values and vars

value :: Parser HExpr
value = HEVal <$> (vint <|> vbool)

vint :: Parser HValue
vint = toVInt <$> integer
  where
    toVInt x = HVInt $ fromInteger x

vbool :: Parser HValue
vbool = (toVTrue <$> try (reserved "True")) <|> (toVFalse <$> try (reserved "False"))
  where
    toVTrue  _ = HVBool True
    toVFalse _ = HVBool False

var :: Parser HExpr
var = HEVar Map.empty <$> identifier

-- main functions

parseExpr :: String -> Either ParseError HExpr
parseExpr = parse expr ""

parseProgram :: String -> Either ParseError HProgram
parseProgram = parse program ""
