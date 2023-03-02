-- |
-- Evaluator
module LambdaDC.Evaluator where

import LambdaDC.Syntax
  ( DelimitedContext (..),
    Expr (..),
    Name,
    Prompt,
    SeqElem (EDC, EPN),
    Sequences,
    Value (Lam, PN, Seq),
  )

subst :: Expr -> Expr -> Name -> Expr
subst = undefined

isValue :: Expr -> Bool
isValue (Val _) = True
isValue _ = False

substHole :: DelimitedContext -> DelimitedContext -> DelimitedContext
substHole DHole dc = dc
substHole (DAppL d e) dc = DAppL (substHole d dc) e
substHole (DAppR v d) dc = DAppR v (substHole d dc)
substHole (DPushPrompt d e) dc = DPushPrompt (substHole d dc) e
substHole (DPushSubCont d e) dc = DPushSubCont (substHole d dc) e
substHole (DWithSubContE d e) dc = DWithSubContE (substHole d dc) e
substHole (DWithSubContP p d) dc = DWithSubContP p (substHole d dc)

delimitedContextToExpr :: DelimitedContext -> Expr -> Expr
delimitedContextToExpr = undefined

evalValue ::
  Value ->
  DelimitedContext ->
  Sequences ->
  Prompt ->
  Maybe (Expr, DelimitedContext, Sequences, Prompt)
evalValue v DHole [] q = Nothing
evalValue v DHole ((EPN p) : sqs) q = return (Val v, DHole, sqs, q)
evalValue v DHole ((EDC dc) : sqs) q = return (Val v, dc, sqs, q)
evalValue v ctx sqs q = return (delimitedContextToExpr ctx (Val v), DHole, sqs, q)

evalExpr ::
  Expr ->
  DelimitedContext ->
  Sequences ->
  Prompt ->
  Maybe (Expr, DelimitedContext, Sequences, Prompt)
evalExpr (Val v) ctx sqs q =
  evalValue v ctx sqs q
evalExpr (App (Val (Lam x e)) v@(Val _)) ctx sqs q =
  return (subst e v x, ctx, sqs, q)
evalExpr (App (Val v) e) ctx sqs q
  | not $ isValue e =
      return (e, substHole ctx (DAppR v DHole), sqs, q)
evalExpr (App e e') ctx sqs q =
  return (e, substHole ctx (DAppL DHole e'), sqs, q)
evalExpr NewPrompt ctx sqs q =
  return (Val (PN q), ctx, sqs, q + 1)
evalExpr (PushPrompt (Val (PN p)) e) ctx sqs q =
  return (e, DHole, EPN p : EDC ctx : sqs, q)
evalExpr (PushPrompt e e') ctx sqs q
  | not $ isValue e =
      return (e, substHole ctx (DPushPrompt DHole e'), sqs, q)
evalExpr (WithSubCont (Val (PN p)) v@(Val _)) ctx sqs q =
  case break (== EPN p) sqs of
    (beforePrompt, _p : afterPrompt) -> return (App v (Val $ Seq (EDC ctx : beforePrompt)), DHole, afterPrompt, q)
    _ -> Nothing
evalExpr (WithSubCont (Val (PN p)) e) ctx sqs q =
  return (e, substHole ctx (DWithSubContP p DHole), sqs, q)
evalExpr (WithSubCont e e') ctx sqs q
  | not $ isValue e =
      return (e, substHole ctx (DWithSubContE DHole e'), sqs, q)
evalExpr (PushSubCont (Val (Seq sqs')) e) ctx sqs q =
  return (e, DHole, sqs' ++ (EDC ctx : sqs), q)
evalExpr (PushSubCont e e') ctx sqs q
  | not $ isValue e =
      return (e, substHole ctx (DPushSubCont DHole e'), sqs, q)
evalExpr _ _ _ _ =
  Nothing
