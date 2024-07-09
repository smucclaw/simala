module Simala.Eval.Type where

import Base
import qualified Base.Text as Text
import Simala.Expr.Type
import Util.RevList

-- * Abstract syntax

data EvalAction =
    Enter Expr
  | Exit Val
  deriving stock Show

data EvalTrace =
  Trace Expr [EvalTrace] Val
  deriving stock Show

data EvalState =
  MkEvalState
    { env     :: Env
    , actions :: RevList EvalAction
    }
  deriving stock (Generic, Show)

data EvalFrame =
  Frame Expr (RevList EvalTrace)

type Eval :: Type -> Type
newtype Eval a =
  MkEval (EvalState -> Either EvalError (a, EvalState))
  deriving
    (Functor, Applicative, Monad, MonadState EvalState, MonadError EvalError)
    via StateT EvalState (ExceptT EvalError Identity)

data EvalError =
    ArityError Int Int -- expected, observed
  | TypeError ValTy ValTy -- expected, observed
  | ScopeError Name
  | DivByZero
  | EmptyListError
  | Crash
  deriving stock Show

-- * Rendering

class Render a where
  render :: a -> String

instance Render Expr where
  render (Builtin b exprs) = render b <> renderArgs exprs
  render (Var x) = Text.unpack x
  render (IfThenElse e1 e2 e3) = "if " <> render e1 <> " then " <> render e2 <> " else " <> render e3
  render (Lit l) = render l
  render (Cons e1 e2) = "cons" <> renderArgs [e1, e2]
  render (List es) = renderList es
  render (Fun t args e) = "fun" <> renderTransparency t <> " " <> renderArgs args <> " => " <> render e
  render (Let t x e1 e2) = "let" <> renderTransparency t <> " " <> render x <> " = " <> render e1 <> " in " <> render e2
  render (App e es) = render e <> renderArgs es
  render Undefined = "undefined"

renderTransparency :: Transparency -> String
renderTransparency Transparent = ""
renderTransparency Opaque      = " opaque"

instance Render Builtin where
  render Minus = "minus"
  render Divide = "divide"
  render Modulo = "modulo"
  render Sum = "sum"
  render Product = "product"
  render Maximum = "maximum"
  render Minimum = "minimum"
  render Not = "not"
  render Lt = "lt"
  render Le = "le"
  render Gt = "gt"
  render Ge = "ge"
  render Eq = "eq"
  render Ne = "ne"
  render And = "and"
  render Or = "ord"
  render Fold = "fold"

instance Render Lit where
  render (IntLit i) = show i
  render (BoolLit True) = "true"
  render (BoolLit False) = "false"

instance Render Val where
  render (VInt i) = show i
  render (VBool b) = show b
  render (VList vs) = renderList vs
  render (VClosure _) = "<fun>"

instance Render Name where
  render x = Text.unpack x

renderArgs :: Render a => [a] -> String
renderArgs xs = "(" <> intercalate "," (map render xs) <> ")"

renderList :: Render a => [a] -> String
renderList xs = "[" <> intercalate "," (map render xs) <> "]"

