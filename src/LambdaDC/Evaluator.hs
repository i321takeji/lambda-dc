-- |
-- Evaluator
module LambdaDC.Evaluator where

import Data.Set (Set)
import Data.Set qualified as Set
import LambdaDC.Syntax
  ( DelimitedContext (..),
    Expr (..),
    Name,
    Prompt,
    SeqElem (..),
    Sequences,
    Value (..),
  )
import Prettyprinter (Pretty (pretty), lbracket, rbracket, slash, (<+>), (<>))

freevars :: Expr -> Set Name
freevars (Val (Var x)) = Set.singleton x
freevars (Val (Lam x e)) = Set.filter (/= x) (freevars e)
freevars (App e1 e2) = Set.union (freevars e1) (freevars e2)
freevars (PushPrompt e1 e2) = Set.union (freevars e1) (freevars e2)
freevars (WithSubCont e1 e2) = Set.union (freevars e1) (freevars e2)
freevars (PushSubCont e1 e2) = Set.union (freevars e1) (freevars e2)
freevars _ = Set.empty

-- freevars' (Val (Seq sqs)) = Set.map freevars' sqs

-- | e[v/x]            ==> subst e v x
--  - x[v/x]           ==> v
--  - x[v/y]           ==> x
--  - (\x.x)[v/x]      ==> \x.x
--  - (\x.\y.z)[v/y]   ==> (\x.\y.z)
--  - (\x.y)[(\z.z)/y] ==> (\x.(\z.z))
--  - (\x.y)[(\z.x)/y] ==> (\x1.y)[(\z.x)/y] ,alpha conversion (\x.y)
subst :: Expr -> Expr -> Name -> Expr
subst e@(Val (Var x)) e' y
  | x == y = e'
  | otherwise = e
subst lam@(Val (Lam x e)) e' y
  | x == y = lam
  | y `notElem` freevars e = lam
  | y `elem` freevars e && x `notElem` freevars e' = Val $ Lam x (subst e e' y)
  | y `elem` freevars e && x `elem` freevars e' =
      error $ show $ pretty "Postpone implements:" <+> pretty lam <+> lbracket <> pretty e' <+> slash <+> pretty y <> rbracket -- alpha conversion for (Lam x e)
subst (App e1 e2) e' y = App (subst e1 e' y) (subst e2 e' y)
subst (PushPrompt e1 e2) e' y = PushPrompt (subst e1 e' y) (subst e2 e' y)
subst (WithSubCont e1 e2) e' y = WithSubCont (subst e1 e' y) (subst e2 e' y)
subst (PushSubCont e1 e2) e' y = PushSubCont (subst e1 e' y) (subst e2 e' y)
subst e _ _ = e

-- subst (Val (Seq sqs)) e' y = Val $ Seq $ map (\s -> substSeqElem s e' y) sqs

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

evalValue :: Value -> DelimitedContext -> Sequences -> Prompt -> Maybe (Expr, DelimitedContext, Sequences, Prompt)
evalValue v DHole [] q = Nothing
evalValue v DHole ((EPN p) : sqs) q = return (Val v, DHole, sqs, q)
evalValue v DHole ((EDC dc) : sqs) q = return (Val v, dc, sqs, q)
evalValue v ctx sqs q = return (delimitedContextToExpr ctx (Val v), DHole, sqs, q)

evalExpr :: Expr -> DelimitedContext -> Sequences -> Prompt -> Maybe (Expr, DelimitedContext, Sequences, Prompt)
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
