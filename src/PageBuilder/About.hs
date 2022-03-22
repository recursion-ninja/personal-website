module PageBuilder.About
    ( buildAbout
    ) where

import Compiler.Constants
import Compiler.HTML
import Data.String
import Hakyll.Core.Identifier.Pattern (Pattern)
import Hakyll.Core.Rules (Rules)
import Hakyll.Web.Template.Context (Context, constField)
import System.FilePath.Posix ((</>))


buildAbout :: Rules ()
buildAbout = compileFormatStaticHTML aboutContext aboutPageInput [templateDefault]


aboutContext :: Context String
aboutContext = contextUsing [constField "NavRef" "about", constField "Title" "About"]


aboutPageInput :: Pattern
aboutPageInput = fromString $ pathToPages </> "about.md"
