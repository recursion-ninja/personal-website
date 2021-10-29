module PageBuilder.Errors
  ( error400PageBuilder
  , error404PageBuilder
  , error500PageBuilder
  ) where

import Compiler.Constants
import Compiler.HTML
import Data.String
import Hakyll
import System.FilePath.Posix ((</>), (<.>))


error400PageBuilder :: Rules ()
error400PageBuilder = mkErrorPage 400


error404PageBuilder :: Rules ()
error404PageBuilder = mkErrorPage 404


error500PageBuilder :: Rules ()
error500PageBuilder = mkErrorPage 500


haikuTemplate :: Identifier
haikuTemplate = fromString $ templatePath </> "haiku.html"


mkErrorPage :: Word -> Rules ()
mkErrorPage n = staticPageBuilder context input templates
  where
    input     = fromString $ pagePath </> show n <.> "html"
    context   = makeContext [ constField "Title" $ show n ]
    templates = [ haikuTemplate, defaultTemplate ]
