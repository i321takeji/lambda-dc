module Test.LambdaDC.Evaluator where

import Data.Set qualified as Set
import LambdaDC.Evaluator (freevars)
import LambdaDC.Syntax (DelimitedContext (..), Expr (..), SeqElem (..), Value (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

test_freevarsTest :: TestTree
test_freevarsTest =
  testGroup
    "free variables"
    [ testCase "Variable" $ freevars (Val $ Var "x") @?= Set.singleton "x",
      testCase "Lambda abstraction 1" $ freevars (Val $ Lam "x" (Val $ Var "x")) @?= Set.empty,
      testCase "Lambda abstraction 2" $ freevars (Val $ Lam "x" (Val $ Var "y")) @?= Set.singleton "y",
      testCase "Prompt" $ freevars (Val $ PN 1) @?= Set.empty,
      testCase "Sequences (Meta continuation) 1" $ freevars (Val $ Seq []) @?= Set.empty,
      testCase "Sequences (Meta continuation) 2" $ freevars (Val $ Seq [EPN 1]) @?= Set.empty,
      testCase "Sequences (Meta continuation) 3" $ freevars (Val $ Seq [EDC DHole]) @?= Set.empty,
      -- testCase "Sequences (Meta continuation) 4" $ freevars (Val $ Seq [EDC (DAppL DHole e)]) @?= Set.empty,
      -- testCase "Sequences (Meta continuation) 5" $ freevars (Val $ Seq [EDC (DAppR v DHole)]) @?= Set.empty,
      -- testCase "Sequences (Meta continuation) 6" $ freevars (Val $ Seq [EDC (DPushPrompt DHole e)]) @?= Set.empty,
      -- testCase "Sequences (Meta continuation) 6" $ freevars (Val $ Seq [EDC (DPushSubCont DHole e)]) @?= Set.empty,
      -- testCase "Sequences (Meta continuation) 6" $ freevars (Val $ Seq [EDC (DWithSubContE DHole e)]) @?= Set.empty,
      testCase "Sequences (Meta continuation) 6" $ freevars (Val $ Seq [EDC (DWithSubContP 1 DHole)]) @?= Set.empty,
      testCase "Application 1" $ freevars (App (Val $ Var "x") (Val $ Var "x")) @?= Set.singleton "x",
      testCase "Application 2" $ freevars (App (Val $ Var "x") (Val $ Var "y")) @?= Set.fromList ["x", "y"],
      testCase "Application 3" $ freevars (App (Val $ Lam "x" (Val $ Var "x")) (Val $ Var "y")) @?= Set.singleton "y",
      testCase "Application 4" $ freevars (App (Val $ Lam "x" (Val $ Var "y")) (Val $ Var "z")) @?= Set.fromList ["y", "z"],
      testCase "newPrompt" $ freevars NewPrompt @?= Set.empty,
      testCase "pushPrompt" $ freevars (PushPrompt (Val $ Var "p") (Val $ Var "x")) @?= Set.fromList ["p", "x"],
      testCase "withSubCont 1" $ freevars (WithSubCont (Val $ Var "p") (Val $ Lam "k" (Val $ Var "x"))) @?= Set.fromList ["p", "x"],
      testCase "withSubCont 2" $ freevars (WithSubCont (Val $ Var "p") (Val $ Lam "k" (PushSubCont (Val $ Var "k") (Val $ Var "x")))) @?= Set.fromList ["p", "x"],
      testCase "pushSubCont" $ freevars (PushSubCont (Val $ Var "k") (Val $ Var "x")) @?= Set.fromList ["k", "x"]
    ]
