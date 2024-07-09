-- | Defines a project-specific "prelude".
--
module Base (module X) where

import Control.Monad as X
import Control.Monad.Except as X
import Control.Monad.Identity as X
import Control.Monad.State as X
import Data.Coerce as X
import Data.Kind as X
import Data.List as X
import Data.Map.Strict as X (Map, (!))
import Data.Maybe as X
import Data.Set as X (Set)
import Data.String as X
import Data.Text as X (Text)
import Data.Void as X
import GHC.Generics as X (Generic)
import Optics.State as X
import Prettyprinter as X (Doc, Pretty(..), (<+>))
