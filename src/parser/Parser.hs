module Parser (parseExpr, parseProgram) where

import Data.Maybe
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))
import Data.Functor (void)
import Control.Monad.Except
import Data.List (sort)
import qualified Data.Map as Map (empty)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Ast
import Lexer

allDifferent :: Ord a => [a] -> Bool
allDifferent = pairwiseDifferent . sort

pairwiseDifferent :: Eq a => [a] -> Bool
pairwiseDifferent xs = and $ zipWith (/=) xs (drop 1 xs)

unfoldAbs :: [HId] -> HExpr -> HExpr
unfoldAbs [i]      e = HEAbs i e
unfoldAbs (i : is) e = HEAbs i $ unfoldAbs is e

unique :: [HId] -> Parser [HId]
unique idents = do
  unless (allDifferent idents) $ fail "conflicting definitions"
  return idents

program :: Parser HProgram
program = do
  void spaces
  bs <- semiSep $ try binding
  let (mn : bsr) = reverse bs
  case mn of
    Bind "main" (HEApp (HEVar _ "print") e) -> return $ Binds (reverse bsr) e
    _                                     -> fail "main function is not detected or it is not last defined function"

expr :: Parser HExpr
expr = Ex.buildExpressionParser table term

term :: Parser HExpr
term = try letIn
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
  args <- many identifier >>= unique
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
  idents <- many1 identifier >>= unique
  reservedOp "->"
  unfoldAbs idents <$> expr

application :: Parser HExpr
application = do
  i          <- var
  app : apps <- sepBy1 operand spaces
  return $ foldl HEApp (HEApp i app) apps

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

parseExpr :: String -> Either ParseError HExpr
parseExpr = parse expr ""

parseProgram :: String -> Either ParseError HProgram
parseProgram = parse program ""
