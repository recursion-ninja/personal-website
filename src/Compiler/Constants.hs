module Compiler.Constants where

import Control.Applicative            (Alternative(empty))
import Data.Foldable
import Data.String
import Data.Time.Clock                (getCurrentTime)
import Data.Time.Format               (defaultTimeLocale, formatTime)
import Data.Version
import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Identifier
import Hakyll.Core.Item
import Hakyll.Core.Metadata
import Hakyll.Core.Provider
import Hakyll.Core.Routes
import Hakyll.Web.Html
import Hakyll.Web.Html.RelativizeUrls
import Hakyll.Web.Template.Context
import Paths_personal_website
import System.FilePath.Posix


baseUrl :: String
baseUrl = "https://recursion.ninja"


pagePath :: FilePath
pagePath = "page"


templatePath :: FilePath
templatePath = "mold"


siteVersion :: Version
siteVersion = version


siteRepository :: String
siteRepository = "https://github.com/recursion-ninja/personal-website"


staticPageRoute :: String -> Routes
staticPageRoute ex = customRoute (takeBaseName . toFilePath) `composeRoutes` setExtension ex


aliasPageRoute :: FilePath -> String -> Routes
aliasPageRoute alias = composeRoutes (constRoute alias) . setExtension


defaultPageRoute :: String -> Routes
defaultPageRoute = setExtension


makeTemplate :: FilePath -> Identifier
makeTemplate = fromString . (templatePath </>)


duplicateField :: String -> String -> Context a
duplicateField original clone = duplicateMapField original clone id


duplicateMapField :: String -> String -> (String -> String) -> Context a
duplicateMapField original clone f = field clone $ \i ->
    maybe empty f . lookupString original <$> getMetadata (itemIdentifier i)


makeContext :: Foldable f => f (Context String) -> Context String
makeContext cs = duplicatedFields <> fold cs <> siteContext


duplicatedFields :: Context String
duplicatedFields = fold
    [ duplicateField "Author" "name"
    , duplicateField "Title"  "title"
    , duplicateField "Date"   "date"
    ]


siteContext :: Context String
siteContext = fold
    [ constField "Author" "Alex Washburn"
    , constField "BaseURL" baseUrl
    , constField "rights" "Creative Commons Non-Commercial Share Alike 3.0"
    , constField "language" "en-US"
    , constField "SiteRepository" siteRepository
    , constField "SiteVersion" $ showVersion siteVersion
    , robustModificationTimeField "LastModified" "%B %e %0Y"
    , robustModificationTimeField "updated"      "%Y-%m-%dT%H:%M:%SZ"
    , routeContext
    ,  fileContext
    , defaultContext
    ]


robustModificationTimeField :: String -> String -> Context String
robustModificationTimeField key fmt = field key $ \i -> do
    let identifier = itemIdentifier i
    provider <- compilerProvider <$> compilerAsk
    modTime  <- if   resourceExists provider identifier
      then pure $ resourceModificationTime provider identifier
      else unsafeCompiler getCurrentTime
    pure $ formatTime defaultTimeLocale fmt modTime


routeContext :: Context String
routeContext =
    field "BaseRoute" $ \i -> do
      let x = itemIdentifier i
          err = fail $ "No route url found for item " <> show x
          f y = let url  = toUrl y
                    root = toSiteRoot url
                in  root </> dropExtension url
      maybe err f <$> getRoute x


fileContext :: Context String
fileContext =
    field "BaseFile" $ \i -> do
      let x = itemIdentifier i
          err = fail $ "No route url found for item " <> show x
      maybe err (takeBaseName . toUrl) <$> getRoute x


defaultTemplate :: Identifier
defaultTemplate = fromString $ templatePath </> "default.html"


pageFinalizer
  :: Item String
  -> Compiler (Item String)
pageFinalizer x = relativizeUrls x >>= saveSnapshot "content"
