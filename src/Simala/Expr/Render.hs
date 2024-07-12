module Simala.Expr.Render where

import Base
import qualified Base.Text as Text

import Simala.Expr.Parser (simpleName)
import Simala.Expr.Type
import Simala.Eval.Type
import Text.Megaparsec (eof, parseMaybe)

class Render a where
  renderAtPrio :: Int -> a -> String
  renderAtPrio _ = render
  render :: a -> String
  render = renderAtPrio 0

  {-# MINIMAL renderAtPrio | render #-}

instance Render EvalError where
  render :: EvalError -> String
  render = show

instance Render Expr where
  renderAtPrio :: Int -> Expr -> String
  renderAtPrio p (Builtin b es)     = renderBuiltin p b es
  renderAtPrio _ (Var x)            = renderName x
  renderAtPrio _ (Atom x)           = "'" <> renderName x
  renderAtPrio _ (Lit l)            = render l
  renderAtPrio p (Cons e1 e2)       = renderBinopr 5 " : " p e1 e2
  renderAtPrio _ (List es)          = renderList es
  renderAtPrio _ (Record r)         = renderRow " = " render r
  renderAtPrio p (Project e n)      = parensIf (p > 9) (renderAtPrio 9 e <> "." <> renderName n)
  renderAtPrio p (Fun t args e)     = parensIf (p > 0) ("fun" <> renderTransparency t <> " " <> renderArgs args <> " => " <> render e)
  renderAtPrio p (Let t x e1 e2)    = parensIf (p > 0) ("let" <> renderTransparency t <> " " <> renderName x <> " = " <> render e1 <> " in " <> render e2)
  renderAtPrio p (Letrec t x e1 e2) = parensIf (p > 0) ("letrec" <> renderTransparency t <> " " <> renderName x <> " = " <> render e1 <> " in " <> render e2)
  renderAtPrio p (App e es)         = parensIf (p > 9) (renderAtPrio 9 e <> renderArgs es)
  renderAtPrio _ Undefined          = "undefined"

renderName :: Name -> String
renderName n
  | needsQuoting n = "`" <> Text.unpack n <> "`"
  | otherwise      = Text.unpack n

needsQuoting :: Name -> Bool
needsQuoting n =
  isNothing (parseMaybe (simpleName <* eof) n)

renderTransparency :: Transparency -> String
renderTransparency Transparent = ""
renderTransparency Opaque      = " opaque"

renderBuiltin :: Int -> Builtin -> [Expr] -> String
renderBuiltin p Minus      [e1, e2]     = renderBinopl 6 " - "  p e1 e2
renderBuiltin p Sum        [e1, e2]     = renderBinopl 6 " + "  p e1 e2
renderBuiltin p Product    [e1, e2]     = renderBinopl 7 " * "  p e1 e2
renderBuiltin p Divide     [e1, e2]     = renderBinopl 7 " / "  p e1 e2
renderBuiltin p Modulo     [e1, e2]     = renderBinopl 7 " % "  p e1 e2
renderBuiltin p Ge         [e1, e2]     = renderBinop  4 " >= " p e1 e2
renderBuiltin p Le         [e1, e2]     = renderBinop  4 " <= " p e1 e2
renderBuiltin p Gt         [e1, e2]     = renderBinop  4 " > "  p e1 e2
renderBuiltin p Lt         [e1, e2]     = renderBinop  4 " < "  p e1 e2
renderBuiltin p Eq         [e1, e2]     = renderBinop  4 " == " p e1 e2
renderBuiltin p Ne         [e1, e2]     = renderBinop  4 " /= " p e1 e2
renderBuiltin p And        [e1, e2]     = renderBinopr 3 " && " p e1 e2
renderBuiltin p Or         [e1, e2]     = renderBinopr 2 " || " p e1 e2
renderBuiltin p IfThenElse [e1, e2, e3] =
  parensIf (p > 0) ("if " <> render e1 <> " then " <> render e2 <> " else " <> render e3)
renderBuiltin _ b          es           = render b <> renderArgs es

renderBinopl :: (Render a1, Render a2) => Int -> String -> Int -> a1 -> a2 -> String
renderBinopl t txt p e1 e2 = parensIf (p > t) (renderAtPrio t e1 <> txt <> renderAtPrio (t + 1) e2)

renderBinopr :: (Render a1, Render a2) => Int -> String -> Int -> a1 -> a2 -> String
renderBinopr t txt p e1 e2 = parensIf (p > t) (renderAtPrio (t + 1) e1 <> txt <> renderAtPrio t e2)

renderBinop :: (Render a1, Render a2) => Int -> String -> Int -> a1 -> a2 -> String
renderBinop t txt p e1 e2 = parensIf (p > t) (renderAtPrio (t + 1) e1 <> txt <> renderAtPrio (t + 1) e2)

parensIf :: Bool -> String -> String
parensIf True  x = "(" <> x <> ")"
parensIf False x = x

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
  render Case       = "case"

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


