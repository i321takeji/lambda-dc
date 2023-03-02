-- |
-- Abstract syntax
module LambdaDC.Syntax
  ( Value (..),
    Expr (..),
    DelimitedContext (..),
    SeqElem (..),
    Name,
    Prompt,
    Sequences,
  )
where

import Prettyprinter (Doc, Pretty (..), brackets, parens, (<+>))

type Name = String

type Prompt = Int

type Sequences = [SeqElem]

data Value
  = -- | Variable
    Var Name
  | -- | Lambda abstraction
    Lam Name Expr
  | -- | Prompt number
    PN Prompt
  | -- | Rest of the context
    Seq Sequences
  deriving (Show, Eq)

data Expr
  = -- | Value
    Val Value
  | -- | Application
    App Expr Expr
  | -- | Create a new prompt
    NewPrompt
  | -- | Delimit the current continuation
    PushPrompt Expr Expr
  | -- | Back to the position `p`, capture the current continuation, and apply `f` to the delimited continuation
    WithSubCont Expr Expr
  | -- | ##
    PushSubCont Expr Expr
  deriving (Show, Eq)

data DelimitedContext
  = DHole
  | DAppL DelimitedContext Expr
  | DAppR Value DelimitedContext
  | DPushPrompt DelimitedContext Expr
  | DPushSubCont DelimitedContext Expr
  | DWithSubContE DelimitedContext Expr
  | DWithSubContP Prompt DelimitedContext
  deriving (Show, Eq)

data SeqElem
  = EPN Prompt
  | EDC DelimitedContext
  deriving (Show, Eq)

instance Pretty SeqElem where
  pretty (EPN p) = prettyPrompt p
  pretty (EDC dc) = pretty dc

instance Pretty DelimitedContext where
  pretty DHole = pretty "@"
  pretty (DAppL dc e) = prettyDelimitedContextParen dc <+> prettyExprParen e
  pretty (DAppR v dc) = parens (pretty v) <+> pretty dc
  pretty (DPushPrompt dc e) = pretty "pushPrompt" <+> pretty dc <+> prettyExprParen e
  pretty (DPushSubCont dc e) = pretty "pushSubCont" <+> pretty dc <+> prettyExprParen e
  pretty (DWithSubContE dc e) = pretty "withSubCont" <+> pretty dc <+> prettyExprParen e
  pretty (DWithSubContP p dc) = pretty "withSubCont PN#" <> pretty p <> pretty dc

prettyDelimitedContextParen :: DelimitedContext -> Doc ann
prettyDelimitedContextParen dc = case dc of
  DAppL _ _ -> parens $ pretty dc
  DAppR _ _ -> parens $ pretty dc
  DPushPrompt _ _ -> parens $ pretty dc
  DWithSubContE _ _ -> parens $ pretty dc
  DWithSubContP _ _ -> parens $ pretty dc
  DPushSubCont _ _ -> parens $ pretty dc
  _ -> pretty dc

lambdaChar :: Char
lambdaChar = 'λ'

prettyPrompt :: Int -> Doc ann
prettyPrompt p = pretty "PN#" <> pretty p

instance Pretty Value where
  pretty (Var v) = pretty v
  pretty (Lam v e) = pretty lambdaChar <> pretty v <> pretty "." <> pretty e
  pretty (PN p) = prettyPrompt p
  pretty (Seq sqs) = brackets $ prettyList sqs

instance Pretty Expr where
  pretty (Val v) = pretty v
  pretty (App e1 e2) = prettyExprParen e1 <+> prettyExprParen e2
  pretty NewPrompt = pretty "newPrompt"
  pretty (PushPrompt e1 e2) = pretty "pushPrompt" <+> prettyExprParen e1 <+> prettyExprParen e2
  pretty (WithSubCont e1 e2) = pretty "withSubCont" <+> prettyExprParen e1 <+> prettyExprParen e2
  pretty (PushSubCont e1 e2) = pretty "pushSubCont" <+> prettyExprParen e1 <+> prettyExprParen e2

prettyExprParen :: Expr -> Doc ann
prettyExprParen e = case e of
  Val (Lam _ _) -> parens $ pretty e
  App _ _ -> parens $ pretty e
  PushPrompt _ _ -> parens $ pretty e
  WithSubCont _ _ -> parens $ pretty e
  PushSubCont _ _ -> parens $ pretty e
  _ -> pretty e
