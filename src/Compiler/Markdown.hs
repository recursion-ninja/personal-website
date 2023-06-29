module Compiler.Markdown
    ( compileFormatMarkdown
    ) where

import Compiler.Generic
import Data.Text (unpack)
import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Item
import Hakyll.Core.Routes
import Hakyll.Core.Rules
import Hakyll.Web.Template.Context
import Text.Pandoc.Writers


compileFormatMarkdown :: Context String -> Pattern -> (String -> Routes) -> [Identifier] -> Rules ()
compileFormatMarkdown = formatCompilerFromSpecification "md" markdownCompiler


markdownCompiler :: Compiler (Item String)
markdownCompiler = fmap unpack <$> compilerFromWriter' "markdownCompiler" writeMarkdown
