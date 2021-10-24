module Compiler.HTML where

import Compiler.Constants
import Compiler.Generic
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Routes
import Hakyll.Core.Rules
import Hakyll.Web.Pandoc
import Hakyll.Web.Template.Context


pageBuilder
  :: Context String
  -> Pattern
  -> (String -> Routes)
  -> [Identifier]
  -> Rules ()
pageBuilder =
    customCompiler "html" pandocCompiler


staticPageBuilder
  :: Context String
  -> Pattern
  -> [Identifier]
  -> Rules ()
staticPageBuilder inputContext inputPath =
    pageBuilder inputContext inputPath staticPageRoute
