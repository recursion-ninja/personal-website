module Compiler.AsciiDoc
  ( compileAsciiDocFormat
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


compileAsciiDocFormat
  :: Context String
  -> Pattern
  -> (String -> Routes)
  -> [Identifier]
  -> Rules ()
compileAsciiDocFormat =
    customCompiler "adoc" asciiDocCompiler


asciiDocCompiler :: Compiler (Item String)
asciiDocCompiler = fmap unpack <$> compilerFromWriter "asciiDocCompiler" writeAsciiDoc
