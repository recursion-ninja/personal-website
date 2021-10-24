module Compiler.EPUB
  ( compileEpubFormat
  ) where

import Compiler.Generic
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Routes
import Hakyll.Core.Rules
import Hakyll.Web.Template.Context
import Text.Pandoc.Writers


compileEpubFormat
  :: Context String
  -> Pattern
  -> (String -> Routes)
  -> Rules ()
compileEpubFormat _ inputPath inputRoute =
    match inputPath . version "epub" $ do
        route   $  inputRoute "epub"
        compile $ compilerFromWriter "epubCompiler" writeEPUB3
