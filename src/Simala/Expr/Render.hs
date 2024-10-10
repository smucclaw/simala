module Simala.Expr.Render where

import Base
import qualified Base.Text as Text

import Simala.Expr.Parser (simpleName)
import Simala.Expr.Type
import Simala.Eval.Type

import Prettyprinter
import Prettyprinter.Internal (Doc(Empty))
import Prettyprinter.Render.Text
import Text.Megaparsec (eof, parseMaybe)

-- | Types that have a human-readable pretty-printed / rendered representation.
class Render a where
  -- | Produce a string rendering of an input given the priority level of the
  -- context. The higher the priority level of the context, the more likely it
  -- is that parentheses will be needed.
  --
  renderAtPrio :: Int -> a -> Doc ann
  renderAtPrio _ = render

  -- | Produce a string rendering of an input in a completely flexible context.
  -- Should never generate parentheses on the outside of the rendering except
  -- if they're a fixed part of the syntax.
  --
  render :: a -> Doc ann
  render = renderAtPrio 0

  {-# MINIMAL renderAtPrio | render #-}

asText :: Doc ann -> Text
asText = renderStrict . layoutPretty defaultLayoutOptions

-- | Smart version of '(<+>)'.
--
-- Only inserts a space if one of the arguments is non-empty.
--
(<<+>>) :: Doc ann -> Doc ann -> Doc ann
Empty <<+>> y = y
x <<+>> Empty = x
x <<+>> y = x <+> y

renderAsText :: Render a => a -> Text
renderAsText = asText . render

instance Render EvalError where
  render :: EvalError -> Doc ann
  render = pretty . show

instance Render Decl where
  renderAtPrio :: Int -> Decl -> Doc ann
  renderAtPrio _ (Rec t name expr)    =
    align (nest 2 (sep ["rec" <<+>> renderTransparency t <<+>> render name <+> "=", render expr]))
  renderAtPrio _ (NonRec t name expr) =
    align (nest 2 (sep [renderTransparency t <<+>> render name <+> "=", render expr]))
  renderAtPrio _ (Eval expr)          =
    "#eval" <+> render expr

instance Render Expr where
  renderAtPrio :: Int -> Expr -> Doc ann
  renderAtPrio p (Builtin b es) = renderBuiltin p b es
  renderAtPrio _ (Var x)        = render x
  renderAtPrio _ (Atom x)       = "'" <> render x
  renderAtPrio _ (Lit l)        = render l
  renderAtPrio _ (Record r)     = renderRow "=" r
  renderAtPrio p (Project e n)  = parensIf (p > 9) (renderAtPrio 9 e <> "." <> render n)
  renderAtPrio p (Fun t args e) =
    parensIf (p > 0)
      (align (nest 2 (sep
        [ "fun" <<+>> renderTransparency t <<+>> renderArgs args <+> "=>"
        , render e
        ]
      )))
  renderAtPrio p e@(Let _ _)    =
    let
      (ds, body) = scanLet e
    in
      parensIf (p > 0)
        (align (sep
          [ "let" <+> align (vsep (punctuate ";" (render <$> ds)))
          , "in" <+> render body
          ]
        ))
  renderAtPrio p (App e es)     = parensIf (p > 9) (renderAtPrio 9 e <> renderArgs es)
  renderAtPrio _ Undefined      = "undefined"

-- | Detect nested lets in order to render them compactly.
--
scanLet :: Expr -> ([Decl], Expr)
scanLet (Let d e) =
  let
    (ds, body) = scanLet e
  in
    (d : ds, body)
scanLet e = ([], e)

-- | Render a name. This will use quotes if and only if they're needed for the
-- name.
--
-- NOTE: Not all names are legal at all, there are still restrictions even on
-- quoted names. So if a name has been generated not by parsing but by creating
-- the AST directly, there's no guarantee that the rendering will be a legal
-- input again.
--
renderName :: Name -> Doc ann
renderName n
  | needsQuoting n = "`" <> pretty n <> "`"
  | otherwise      = pretty n

-- | For safety, we use the actual parser for "simple" names to decide whether
-- a name needs quoting when rendered.
--
needsQuoting :: Name -> Bool
needsQuoting n =
  isNothing (parseMaybe (simpleName <* eof) n)

instance Render Transparency where
  render = renderTransparency

-- | As 'Transparent' is the default transparency, we only render 'Opaque'
-- transparency annotations.
--
renderTransparency :: Transparency -> Doc ann
renderTransparency Transparent = mempty
renderTransparency Opaque      = "opaque"

renderBuiltin :: Int -> Builtin -> [Expr] -> Doc ann
renderBuiltin p Minus      [e1, e2]     = renderBinopl 6 "-"  p e1 e2
renderBuiltin p Sum        [e1, e2]     = renderBinopl 6 "+"  p e1 e2
renderBuiltin p Product    [e1, e2]     = renderBinopl 7 "*"  p e1 e2
renderBuiltin p Divide     [e1, e2]     = renderBinopl 7 "/"  p e1 e2
renderBuiltin p Modulo     [e1, e2]     = renderBinopl 7 "%"  p e1 e2
renderBuiltin p Append     [e1, e2]     = renderBinopr 6 "++" p e1 e2
renderBuiltin p Ge         [e1, e2]     = renderBinop  4 ">=" p e1 e2
renderBuiltin p Le         [e1, e2]     = renderBinop  4 "<=" p e1 e2
renderBuiltin p Gt         [e1, e2]     = renderBinop  4 ">"  p e1 e2
renderBuiltin p Lt         [e1, e2]     = renderBinop  4 "<"  p e1 e2
renderBuiltin p Eq         [e1, e2]     = renderBinop  4 "==" p e1 e2
renderBuiltin p HEq        [e1, e2]     = renderBinop  4 "~=" p e1 e2
renderBuiltin p Ne         [e1, e2]     = renderBinop  4 "/=" p e1 e2
renderBuiltin p And        [e1, e2]     = renderBinopr 3 "&&" p e1 e2
renderBuiltin p Or         [e1, e2]     = renderBinopr 2 "||" p e1 e2
renderBuiltin p IfThenElse [e1, e2, e3] =
  parensIf (p > 0) ("if" <+> render e1 <+> "then" <+> render e2 <+> "else" <+> render e3)
renderBuiltin p Cons       [e1, e2]     = renderBinopr 5 " : " p e1 e2
renderBuiltin _ List       es           = renderList es
renderBuiltin _ b          es           = render b <> renderArgs es

renderBinopl :: (Render a1, Render a2) => Int -> Text -> Int -> a1 -> a2 -> Doc ann
renderBinopl t txt p e1 e2 =
  parensIf (p > t)
    (align (sep
      [ gindent (Text.length txt + 1) (renderAtPrio t e1)
      , pretty txt <+> renderAtPrio (t + 1) e2
      ]
    ))

renderBinopr :: (Render a1, Render a2) => Int -> Text -> Int -> a1 -> a2 -> Doc ann
renderBinopr t txt p e1 e2 =
  parensIf (p > t)
    (align (sep
      [ gindent (Text.length txt + 1) (renderAtPrio (t + 1) e1)
      , pretty txt <> renderAtPrio t e2
      ]
    ))

renderBinop :: (Render a1, Render a2) => Int -> Text -> Int -> a1 -> a2 -> Doc ann
renderBinop t txt p e1 e2 =
  parensIf (p > t)
    (align (sep
      [ gindent (Text.length txt + 1) (renderAtPrio (t + 1) e1)
      , pretty txt <> renderAtPrio (t + 1) e2
      ]
    ))

gindent :: Int -> Doc ann -> Doc ann
gindent i doc = flatAlt (indent i doc) doc

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True  x = "(" <> x <> ")"
parensIf False x = x

instance Render Builtin where
  render :: Builtin -> Doc ann
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
  render HEq        = "heq"
  render Ne         = "ne"
  render And        = "and"
  render Or         = "or"
  render IfThenElse = "ifthenelse"
  render Foldr      = "foldr"
  render Foldl      = "foldl"
  render Case       = "case"
  render Merge      = "merge"
  render Floor      = "floor"
  render Ceiling    = "ceiling"
  render FromInt    = "fromInt"
  render Explode    = "explode"
  render Append     = "append"
  render TypeOf     = "typeOf"
  render Cons       = "cons"
  render List       = "list"

instance Render Lit where
  render :: Lit -> Doc ann
  render (IntLit i)      = pretty (show i)
  render (BoolLit True)  = "true"
  render (BoolLit False) = "false"
  render (StringLit s)   = pretty (show s)
  render (FracLit f)     = pretty (show f)

instance Render Val where
  render :: Val -> Doc ann
  render (VInt i)                          = pretty (show i)
  render (VBool True)                      = "true"
  render (VBool False)                     = "false"
  render (VString s)                       = pretty (show s)
  render (VFrac f)                         = pretty (show f)
  render (VList vs)                        = renderList vs
  render (VRecord r)                       = renderRow "=" r
  render (VClosure (MkClosure t args _ _)) = "<fun" <> renderTransparency t <> "/" <> pretty (show (length args)) <> ">"
  render (VAtom x)                         = "'" <> render x
  render VBlackhole                        = "<blackhole>"

instance Render Name where
  render :: Name -> Doc ann
  render = renderName

-- | Helper function to render an argument / parameter list.
renderArgs :: Render a => [a] -> Doc ann
renderArgs xs = "(" <> align (cat (punctuate "," (map render xs))) <> ")"

-- | Helper function to render a literal list.
renderList :: Render a => [a] -> Doc ann
renderList xs = "[" <> align (cat (punctuate "," (map render xs))) <> "]"

-- | Helper function to render a row. Takes as argument the
-- string that separates names from payloads.
--
renderRow :: forall a ann. Render a => Doc ann -> Row a -> Doc ann
renderRow sepd xs =
  "{" <> align (cat (punctuate "," (map item xs))) <> "}"
  where
    item :: (Name, a) -> Doc ann
    item (x, a) = render x <+> sepd <+> render a
