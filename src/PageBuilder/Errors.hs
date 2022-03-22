module PageBuilder.Errors
    ( buildError400
    , buildError404
    , buildError500
    ) where

import Compiler.Constants
import Compiler.HTML
import Data.String
import Hakyll.Core.Identifier (Identifier)
import Hakyll.Core.Rules (Rules)
import Hakyll.Web.Template.Context (constField)
import System.FilePath.Posix ((<.>), (</>))


buildError400 :: Rules ()
buildError400 = mkErrorPage 400


buildError404 :: Rules ()
buildError404 = mkErrorPage 404


buildError500 :: Rules ()
buildError500 = mkErrorPage 500


haikuTemplate :: Identifier
haikuTemplate = fromString $ pathToTemplates </> "haiku.html"


mkErrorPage :: Word -> Rules ()
mkErrorPage n = compileFormatStaticHTML context input templates
    where
        input     = fromString $ pathToPages </> show n <.> "html"
        context   = contextUsing [constField "Title" $ show n]
        templates = [haikuTemplate, templateDefault]
