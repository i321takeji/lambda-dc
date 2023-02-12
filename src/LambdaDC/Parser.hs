{-# LANGUAGE OverloadedStrings #-}

module LambdaDC.Parser where

import Control.Monad (guard)
import Data.Text (Text)
import Data.Void (Void)
import LambdaDC.Syntax (Expr (..), Value (Lam, Var))
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    between,
    choice,
    many,
    oneOf,
    some,
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    lowerChar,
    space,
    string,
  )

type Parser = Parsec Void Text

pVar :: Parser String
pVar = do
  c <- lowerChar <|> char '_'
  cs <- many alphaNumChar
  let str = c : cs
  guard $ str `notElem` ["newPrompt", "pushPrompt", "withSubCont", "pushSubCont"]
  space
  return (c : cs)

pExprVar :: Parser Expr
pExprVar = Val . Var <$> pVar

pToken :: Parser a -> Parser ()
pToken p = p >> space

pLambda :: Parser Expr
pLambda = do
  pToken $ oneOf ['\\', 'λ']
  x <- pVar
  pToken $ char '.'
  e <- pExpr
  return $ Val (Lam x e)

pNewPrompt :: Parser Expr
pNewPrompt = do
  pToken $ string "newPrompt"
  return NewPrompt

pPushPrompt :: Parser Expr
pPushPrompt = do
  pToken $ string "pushPrompt"
  PushPrompt <$> pTerm <*> pExpr

pWithSubCont :: Parser Expr
pWithSubCont = do
  pToken $ string "withSubCont"
  WithSubCont <$> pTerm <*> pExpr

pPushSubCont :: Parser Expr
pPushSubCont = do
  pToken $ string "pushSubCont"
  PushSubCont <$> pTerm <*> pExpr

pParen :: Parser Expr
pParen =
  between
    (pToken $ char '(')
    (pToken $ char ')')
    pExpr

pTerm :: Parser Expr
pTerm = choice $ map try [pNewPrompt, pPushPrompt, pWithSubCont, pPushSubCont, pLambda, pParen, pExprVar]

pExpr :: Parser Expr
pExpr = do
  (e : es) <- some pTerm
  return $ foldl App e es
