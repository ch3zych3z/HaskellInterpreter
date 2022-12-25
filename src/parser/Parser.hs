{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parser (parseExpr, parseProgram, parsePrelude) where

import Prelude hiding (maybe)
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
   <|> try just
   <|> try caseOf
   <|> try application
   <|> try abstraction
   <|> try ifelse
   <|> operand

operand :: Parser HExpr
operand = 
      try (parens (try tuple))
  <|> try nothing
  <|> try (parens (try expr))
  <|> try value 
  <|> try var 
  <|> try list 

table = [ [ binIntOp "*" Mul,
            binIntOp "`div`" Div,
            binIntOp "`mod`" Rem]
        , [ binIntOp "+" Add,
            binIntOp "-" Sub]
        , [ consOp ]
        , [ compOp "==" Eqls,
            compOp ">" Gr,
            compOp "<" Le,
            compOp ">=" Greq,
            compOp "<=" Leq
            ]
        ]

binary name fun = Ex.Infix ( do { reservedOp name; return fun } )

binaryAssoc as s op = binary s (`HEBinOp` op) as

binIntOp = binaryAssoc Ex.AssocLeft

compOp = binaryAssoc Ex.AssocNone

consOp = binary ":" consExpr Ex.AssocRight

nothing :: Parser HExpr
nothing = do
  reserved "Nothing"
  return $ HETypeCons $ HCMaybe Nothing

just :: Parser HExpr
just = do
  reserved "Just"
  e <- operand
  return $ HETypeCons $ HCMaybe $ Just e

tuple :: Parser HExpr
tuple = let
  tupleEl = expr <* reservedOp ","
  in (\ps p -> HETypeCons $ HCTuple (length ps + 1) (ps ++ [p])) <$> (many1 $ try tupleEl) <*> try expr

list :: Parser HExpr
list = do
  es <- brackets $ commaSep expr
  return $ foldr consExpr nilExpr es

caseOf :: Parser HExpr
caseOf = do
  reserved "case"
  e <- expr
  reserved "of"
  ms <- braces $ semiSep matching
  return $ HECase e ms

matching :: Parser Matching
matching = do
  p <- pattern
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
  args <- many $ try operandPat
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
  pats <- many1 $ try operandPat
  unique pats
  reservedOp "->"
  unfoldAbs pats <$> expr

application :: Parser HExpr
application = let
  app = spaces $> HEApp
  in chainl1 operand app

-- patterns
pattern :: Parser HPattern
pattern =
      try justPat
  <|> try operandPat

operandPat :: Parser HPattern
operandPat =
      try listPat
  <|> try nothingPat
  <|> try labelPat
  <|> try valuePat
  <|> try identPat
  <|> try wildcardPat
  <|> try (parens tuplePat)
  <|> try (parens pattern)

nothingPat :: Parser HPattern
nothingPat = do
  reserved "Nothing"
  return $ HPMaybe Nothing

justPat :: Parser HPattern
justPat = do
  reserved "Just"
  p <- operandPat
  return $ HPMaybe $ Just p
    

tuplePat :: Parser HPattern
tuplePat = let 
  tupleEl = pattern <* reservedOp "," 
  in (\ps p -> HPTuple (length ps + 1) (ps ++ [p])) <$> (many1 $ try tupleEl) <*> try pattern

listPat :: Parser HPattern
listPat = do
  let
    listVal  = do
      ps <- brackets $ commaSep pattern
      return $ unfoldCons (HPList HLPNil) ps

    unfoldCons :: HPattern -> [HPattern] -> HPattern
    unfoldCons = foldr (\h t -> HPList $ HLPCons h t)

    listCons = do
      ps <- parens $ sepBy1 pattern (reservedOp ":")
      let (ini, lt) = (init ps, last ps)
      return $ unfoldCons lt ini
  try listVal <|> try listCons

wildcardPat :: Parser HPattern
wildcardPat = reserved "_" $> HPWildcard

labelPat :: Parser HPattern
labelPat = do
  let
    lbl = do
      i <- identifier
      reservedOp "@"
      return i
  ls <- many1 $ try lbl
  HPLabel ls <$> operandPat

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

prelude :: Parser [Binding]
prelude = semiSep $ try binding

parsePrelude :: String -> HProgram -> HProgram
parsePrelude s binds = case parse prelude "" s of
  ~(Right bs') -> Binds bs' $ HELet binds

parseExpr :: String -> Either ParseError HExpr
parseExpr = parse expr ""

parseProgram :: String -> Either ParseError HProgram
parseProgram = parse program ""
