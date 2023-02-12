{-# LANGUAGE OverloadedStrings #-}

module LambdaDC.Parser where

import Control.Monad (guard)
import Data.Text (Text)
import Data.Void (Void)
import LambdaDC.Syntax (Expr (App, NewPrompt, PushPrompt, PushSubCont, Val, WithSubCont), Value (Lam, Var))
import Text.Megaparsec (MonadParsec (try), Parsec, between, choice, many, manyTill, parse, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, space, string)

type Parser = Parsec Void Text

pVar :: Parser String
pVar = do
  c <- lowerChar <|> char '_'
  cs <- many alphaNumChar
  let str = c : cs
  guard $ str `notElem` ["newPrompt", "pushPrompt", "withSubCont", "pushSubCont"]
  space
  return (c : cs)

pValVar :: Parser Expr
pValVar = Val . Var <$> pVar

pToken :: Parser a -> Parser ()
pToken p = p >> space

pLambda :: Parser Expr
pLambda = do
  pToken $ char '\\' <|> char 'λ'
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
  PushPrompt <$> pExpr <*> pExpr

pWithSubCont :: Parser Expr
pWithSubCont = do
  pToken $ string "withSubCont"
  WithSubCont <$> pExpr <*> pExpr

pPushSubCont :: Parser Expr
pPushSubCont = do
  pToken $ string "pushSubCont"
  PushSubCont <$> pExpr <*> pExpr

pParen :: Parser Expr
pParen =
  between
    (pToken $ char '(')
    (pToken $ char ')')
    pExpr

pExpr :: Parser Expr
pExpr = choice [pLambda, pParen, pNewPrompt, pPushPrompt, pWithSubCont, pPushSubCont, pValVar]

-- left recursion
pApp :: Parser Expr
pApp = App <$> pExpr <*> pExpr
