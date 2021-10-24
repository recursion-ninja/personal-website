module Compiler.Textual
  ( compileTextualFormat
  ) where

import Compiler.Generic
import Data.Text                      (unpack)
import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Item
import Hakyll.Core.Routes
import Hakyll.Core.Rules
import Hakyll.Web.Template.Context
import Text.Pandoc.Writers


compileTextualFormat
  :: Context String
  -> Pattern
  -> (String -> Routes)
  -> [Identifier]
  -> Rules ()
compileTextualFormat =
    customCompiler "txt" textualCompiler


textualCompiler :: Compiler (Item String)
textualCompiler = fmap unpack <$> compilerFromWriter "textualCompiler" writePlain
