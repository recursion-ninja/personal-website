module Compiler.Markdown
  ( compileMarkdownFormat
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


compileMarkdownFormat
  :: Context String
  -> Pattern
  -> (String -> Routes)
  -> [Identifier]
  -> Rules ()
compileMarkdownFormat =
    customCompiler "md" markdownCompiler


markdownCompiler :: Compiler (Item String)
markdownCompiler = fmap unpack <$> compilerFromWriter "markdownCompiler" writeMarkdown
