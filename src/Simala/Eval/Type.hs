module Simala.Eval.Type where

import Base
import qualified Base.Text as Text
import Simala.Expr.Type
import Util.RevList

-- * Abstract syntax

data EvalAction =
    Enter Expr
  | Exit Val
  | Exception EvalError
  deriving stock Show

data EvalTrace =
  Trace Expr [EvalTrace] (Either EvalError Val)
  deriving stock Show

data EvalState =
  MkEvalState
    { env          :: !Env
    , actions      :: RevList EvalAction
    , transparency :: !Transparency
    }
  deriving stock (Generic, Show)

data EvalFrame =
  Frame Expr (RevList EvalTrace)

type Eval :: Type -> Type
newtype Eval a =
  MkEval (EvalState -> (Either EvalError a, EvalState))
  deriving
    (Functor, Applicative, Monad, MonadState EvalState, MonadError EvalError)
    via ExceptT EvalError (StateT EvalState Identity)

data EvalError =
    ArityError Int Int -- expected, observed
  | TypeError ValTy ValTy -- expected, observed
  | ScopeError Name
  | DivByZero
  | EmptyListError
  | RecordProjectionError Name
  | Crash
  deriving stock Show

-- * Rendering

class Render a where
  render :: a -> String

instance Render EvalError where
  render :: EvalError -> String
  render = show

instance Render Expr where
  render :: Expr -> String
  render (Builtin b exprs)  = render b <> renderArgs exprs
  render (Var x)            = Text.unpack x
  render (Atom x)           = "'" <> Text.unpack x
  render (Lit l)            = render l
  render (Cons e1 e2)       = "cons" <> renderArgs [e1, e2]
  render (List es)          = renderList es
  render (Record r)         = renderRow " = " render r
  render (Project r n)      = render r <> "." <> Text.unpack n
  render (Fun t args e)     = "fun" <> renderTransparency t <> " " <> renderArgs args <> " => " <> render e
  render (Let t x e1 e2)    = "let" <> renderTransparency t <> " " <> render x <> " = " <> render e1 <> " in " <> render e2
  render (Letrec t x e1 e2) = "letrec" <> renderTransparency t <> " " <> render x <> " = " <> render e1 <> " in " <> render e2
  render (App e es)         = render e <> renderArgs es
  render Undefined          = "undefined"

renderTransparency :: Transparency -> String
renderTransparency Transparent = ""
renderTransparency Opaque      = " opaque"

instance Render Builtin where
  render :: Builtin -> String
  render Minus      = "minus"
  render Divide     = "divide"
  render Modulo     = "modulo"
  render Sum        = "sum"
  render Product    = "product"
  render Maximum    = "maximum"
  render Minimum    = "minimum"
  render Not        = "not"
  render Lt         = "lt"
  render Le         = "le"
  render Gt         = "gt"
  render Ge         = "ge"
  render Eq         = "eq"
  render Ne         = "ne"
  render And        = "and"
  render Or         = "or"
  render IfThenElse = "ifthenelse"
  render Foldr      = "foldr"
  render Foldl      = "foldl"

instance Render Lit where
  render :: Lit -> String
  render (IntLit i)      = show i
  render (BoolLit True)  = "true"
  render (BoolLit False) = "false"

instance Render Val where
  render :: Val -> String
  render (VInt i)                          = show i
  render (VBool True)                      = "true"
  render (VBool False)                     = "false"
  render (VList vs)                        = renderList vs
  render (VRecord r)                       = renderRow " = " render r
  render (VClosure (MkClosure t args _ _)) = "<fun" <> renderTransparency t <> "(" <> show (length args) <> ")>"
  render (VAtom x)                         = "'" <> Text.unpack x
  render VBlackhole                        = "<blackhole>"

instance Render Name where
  render :: Name -> String
  render x = Text.unpack x

renderArgs :: Render a => [a] -> String
renderArgs xs = "(" <> intercalate "," (map render xs) <> ")"

renderList :: Render a => [a] -> String
renderList xs = "[" <> intercalate "," (map render xs) <> "]"

renderRow :: forall a. String -> (a -> String) -> Row a -> String
renderRow sep payload xs =
  "{" <> intercalate "," (map item xs) <> "}"
  where
    item :: (Name, a) -> String
    item (x, a) = Text.unpack x <> sep <> payload a


