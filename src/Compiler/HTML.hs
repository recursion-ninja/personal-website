module Compiler.HTML
    ( pageBuilderWith
    , pageBuilder
    , staticPageBuilder
    ) where

import Compiler.Constants
import Compiler.Generic
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Routes
import Hakyll.Core.Rules
import Hakyll.Web.Pandoc
import Hakyll.Web.Template.Context
import Text.Pandoc.Definition


pageBuilderWith
  :: (Pandoc -> Pandoc)
  -> Context String
  -> Pattern
  -> (String -> Routes)
  -> [Identifier]
  -> Rules ()
pageBuilderWith transform =
    let compiler = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions transform
    in  customCompiler "html" compiler


pageBuilder
  :: Context String
  -> Pattern
  -> (String -> Routes)
  -> [Identifier]
  -> Rules ()
pageBuilder = pageBuilderWith id


staticPageBuilder
  :: Context String
  -> Pattern
  -> [Identifier]
  -> Rules ()
staticPageBuilder inputContext inputPath =
    pageBuilder inputContext inputPath staticPageRoute
