module AssetIncludes
    ( includeCSS
    , includeImages
    , includeFavicons
    , includeTemplates
    ) where

import Compiler.Constants
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Data.String (fromString)
import Hakyll.Core.Compiler (Compiler)
import Hakyll.Core.File (copyFileCompiler)
import Hakyll.Core.Identifier.Pattern (Pattern)
import Hakyll.Core.Item (Item)
import Hakyll.Core.Rules (Rules, compile, match, route)
import Hakyll.Core.Writable (Writable)
import Hakyll.Web.CompressCss (compressCssCompiler)
import Hakyll.Web.Template (Template, templateBodyCompiler)
import System.FilePath.Posix ((</>))


includeCSS, includeImages, includeFavicons, includeTemplates :: Rules ()
includeCSS       = pathToCSS        `assetsIncludedUsing` compressCssCompiler
includeImages    = pathToImages     `assetsIncludedUsing` copyFileCompiler
includeFavicons  = pathToFavicons   `assetsIncludedUsing` copyFileCompiler
includeTemplates = pathToTemplates `designsIncludedUsing` templateBodyCompiler


allWithin :: FilePath -> Pattern
allWithin = fromString . (</> "*")


assetsIncludedUsing
  :: ( Binary a
     , Typeable a
     , Writable a
     )
  => FilePath
  -> Compiler (Item a)
  -> Rules ()
assetsIncludedUsing pathToResources compiler =
   match (allWithin pathToResources) $ do
        route   pageRouteFromDataDirectory
        compile compiler


designsIncludedUsing :: FilePath -> Compiler (Item Template) -> Rules ()
designsIncludedUsing path compiler = match (allWithin path) $ compile compiler
