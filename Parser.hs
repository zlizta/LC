module Parser
  ( parse
  , Phrase (..)
  , sPhrase
  , sProg
  , s2Terms
  , sTerm )
  where

import Prelude hiding (pi)
import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Data.List
import Text.Parsec.Combinator
import Text.Parsec.Error ( ParseError )
import Text.Parsec.Pos   ( SourceName )
import Text.Parsec.Prim hiding ( label, parse, token )
import qualified Text.ParserCombinators.Parsec as Parsec ( parse )
import Text.ParserCombinators.Parsec.Char ( string )
import qualified Data.Set as Set
import qualified Data.Map as Map

import Lexer
import Location
import Smart
import Syntax


parse :: Parser a -> SourceName -> String -> Either ParseError a
parse p = Parsec.parse (whiteSpace *> p <* eof)

data Phrase
  = Prog Prog
  | Term Term
  deriving (Show, Eq)

sPhrase :: Parser Phrase
sPhrase = choice
  [ try $ Prog <$> sProg <* eof
  ,       Term <$> sTerm <* eof
  ]

sProg :: Parser Prog
sProg = concat <$> (sEntry `sepEndBy` semi)

s2Terms :: Parser (Term, Term)
s2Terms = (,) <$> atom <*> atom

sTerm :: Parser Term
sTerm = choice
  [ Let   <$> locReserved "let"
          <*> sProg
          <*     reserved "in"
          <*> sTerm

  , lam   <$  tokLam
          <*> many1 ((,) <$> location <*> sName)
          <*  tokArr
          <*> sTerm

  , split <$> locReserved "split"
          <*> sTerm
          <*     reserved "with"
          <*> parens ((,) <$> sName
                          <*  comma
                          <*> sName)
          <*  tokArr
          <*> sTerm

  , try (Unfold <$> locReserved "unfold"
                <*> sTerm
                <*  reserved "as")
                <*> ((,) <$> sName
                         <*  tokArr
                         <*> sTerm)

  , try (do (ns, t) <- binder
            tokArr
            return $ pis ns t) <*> sTerm

  , productOrHigher `andMaybe` (flip <$> functionArrow <*> sTerm)
  ]

-- | @p `andMaybe` q@ parses a @p@ and then maybe a @q@. The result of
-- @q@, if any, is applied to the result of @p@.

andMaybe :: Parser a -> Parser (a -> a) -> Parser a
p `andMaybe` q = (\a -> maybe a ($ a)) <$> p <*> optionMaybe q

sLabel :: Parser Name
sLabel  =  string "'" *> identifier

sName  :: Parser Name
sName   =  identifier

-- * Terms

prefixOperator :: Parser (Term -> Term)
prefixOperator = choice
  [ Rec    <$> locReserved "Rec"
  , Fold   <$> locReserved "fold"
  , Lift   <$> tokLift
  , Force  <$> tokForce
  , Box    <$> locReservedOp "♯"
  , unfold <$> locReserved "unfold"
  ]
  where
  unfold l t = Unfold l t id
  id         = ( " x"
               , Var Unknown " x"
               )

productOperator :: Parser (Term -> Term -> Term)
productOperator = (-*-) <$ locReservedOp "*"

functionArrow :: Parser (Term -> Term -> Term)
functionArrow = (->-) <$ tokArr

atom :: Parser Term
atom = choice
  [ try $ parens sTerm

  , Type  <$> locReserved "Type"

  , pair  <$>     location
              <*> parens ((,) <$> sTerm <* comma <*> sTerm)
          <?> "pair"

  , Enum  <$>     location
              <*> braces (Set.fromList <$> many sName)
          <?> "enumeration"

  , Lab   <$>     location
              <*> sLabel
          <?> "label"

  , Case  <$>     locReserved "case"
              <*> sTerm
              <*  reserved    "of"
              <*> braces (Map.fromList <$> sBranch `sepBy` locReservedOp "|") -- TO DO : detect  repetitions of cases and raise error

  , Box   <$>     location
              <*> brackets sTerm
          <?> "box"

  , Var   <$> location <*> sName
  ]
  where pair = uncurry . Pair

atomOrApplication :: Parser Term
atomOrApplication =
  foldl App <$> (atom <|> prefixOperator <*> atom) <*> many atom

productOrHigher :: Parser Term
productOrHigher = choice
  [ try (do (ns, t) <- binder
            productOperator
            return $ sigmas ns t) <*> productOrHigher
  , atomOrApplication `andMaybe`
      (flip <$> productOperator <*> productOrHigher)
  ]

binder :: Parser ([(Location, Name)], Type)
binder = parens $ (,) <$> many1 ((,) <$> location <*> sName)
                      <*  reservedOp ":"
                      <*> sTerm

sEntry :: Parser [ProgramEntry]
sEntry =
    do
      l <- location
      n <- sName
      b <- choice [ True  <$ reservedOp ":"
                  , False <$ reservedOp "="
                  ]
      t <- sTerm
      if b
        then do
               d <- option Nothing (Just <$ reservedOp "=" <*> sTerm)
               case d of
                 Nothing -> return [Decl l n t]
                 Just t' -> return [Decl l n t, Defn l n t']
        else return [Defn l n t]

sBranch :: Parser (Label,Term)
sBranch = (,) <$> sName <* tokArr <*> sTerm

