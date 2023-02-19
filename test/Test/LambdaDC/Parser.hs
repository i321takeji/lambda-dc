{-# LANGUAGE OverloadedStrings #-}

module Test.LambdaDC.Parser where

import LambdaDC.Parser (parseExpr)
import LambdaDC.Syntax (Expr (..), Value (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

test_parseTest :: TestTree
test_parseTest =
  testGroup
    "parse expression"
    [ testCase "parse Variable" $ parseExpr "x" @?= Right (Val $ Var "x"),
      testCase "parse Lambda abstraction" $ parseExpr "λx.x" @?= Right (Val $ Lam "x" (Val $ Var "x")),
      testCase "parse Lambda abstraction" $ parseExpr "λx.x y" @?= Right (Val $ Lam "x" (App (Val $ Var "x") (Val $ Var "y"))),
      testCase "parse Application" $ parseExpr "f x" @?= Right (App (Val $ Var "f") (Val $ Var "x")),
      testCase "parse Application" $ parseExpr "(λx.x) y" @?= Right (App (Val $ Lam "x" (Val $ Var "x")) (Val $ Var "y")),
      testCase "parse newPrompt" $ parseExpr "newPrompt" @?= Right NewPrompt,
      testCase "parse pushPrompt" $ parseExpr "pushPrompt p x" @?= Right (PushPrompt (Val $ Var "p") (Val $ Var "x")),
      testCase "parse withSubCont" $ parseExpr "withSubCont p (λk.k)" @?= Right (WithSubCont (Val $ Var "p") (Val $ Lam "k" (Val $ Var "k"))),
      testCase "parse pushSubCont" $ parseExpr "pushSubCont k x" @?= Right (PushSubCont (Val $ Var "k") (Val $ Var "x")),
      testCase "parse example1" $
        parseExpr "(λp.pushPrompt p ((λy.withSubCont p (λk.pushSubCont k x)))) newPrompt"
          @?= Right
            ( App
                ( Val
                    ( Lam
                        "p"
                        ( PushPrompt
                            (Val $ Var "p")
                            (Val (Lam "y" (WithSubCont (Val $ Var "p") (Val (Lam "k" (PushSubCont (Val $ Var "k") (Val $ Var "x")))))))
                        )
                    )
                )
                NewPrompt
            )
    ]
