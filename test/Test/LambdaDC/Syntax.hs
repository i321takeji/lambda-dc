module Test.LambdaDC.Syntax where

import LambdaDC.Syntax (Expr (..), Value (Lam, Var))
import Prettyprinter (Pretty (pretty))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | x
example1 :: Expr
example1 = Val $ Var "x"

-- | \x.x
example2 :: Expr
example2 = Val $ Lam "x" (Val $ Var "x")

-- | (\x.x) y
example3 :: Expr
example3 = App (Val $ Lam "x" $ Val $ Var "x") (Val $ Var "y")

-- | (\p.pushPrompt p ((\y.withSubCont p (\k.pushSubCont k x))) newPrompt
example4 :: Expr
example4 =
  App
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

-- | (\p.(\x.x) (pushPrompt p ((\y1.\y2.\1) (withSubCont p (\k.pushSubCont k z))))) newPrompt",
example5 :: Expr
example5 =
  App
    ( Val $
        Lam
          "p"
          ( App
              (Val $ Lam "x" (Val $ Var "x"))
              ( PushPrompt
                  (Val $ Var "p")
                  ( App
                      (Val $ Lam "y1" (Val $ Lam "y2" (Val $ Var "y1")))
                      (WithSubCont (Val $ Var "p") (Val $ Lam "k" (PushSubCont (Val $ Var "k") (Val $ Var "z"))))
                  )
              )
          )
    )
    NewPrompt

-- | (\p.(λx.x) (pushPrompt p ((\y1.\y2.y1) (withSubCont p (\k.z))))) newPrompt
example6 :: Expr
example6 =
  App
    ( Val $
        Lam
          "p"
          ( App
              (Val $ Lam "x" (Val $ Var "x"))
              ( PushPrompt
                  (Val $ Var "p")
                  ( App
                      (Val $ Lam "y1" (Val $ Lam "y2" (Val $ Var "y1")))
                      (WithSubCont (Val $ Var "p") (Val $ Lam "k" (Val $ Var "z")))
                  )
              )
          )
    )
    NewPrompt

test_prettyTest :: TestTree
test_prettyTest =
  testGroup
    "pretty-print expression"
    [ testCase "prints Variable" $ show (pretty example1) @?= "x",
      testCase "prints Lambda abstraction" $ show (pretty example2) @?= "λx.x",
      testCase "prints Application" $ show (pretty example3) @?= "(λx.x) y",
      testCase "prints DC operator 1" $ show (pretty example4) @?= "(λp.pushPrompt p (λy.withSubCont p (λk.pushSubCont k x))) newPrompt",
      testCase "prints DC operator 2" $ show (pretty example5) @?= "(λp.(λx.x) (pushPrompt p ((λy1.λy2.y1) (withSubCont p (λk.pushSubCont k z))))) newPrompt",
      testCase "prints DC operator 3" $ show (pretty example6) @?= "(λp.(λx.x) (pushPrompt p ((λy1.λy2.y1) (withSubCont p (λk.z))))) newPrompt"
    ]
