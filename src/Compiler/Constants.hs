{-# LANGUAGE LambdaCase #-}

module Compiler.Constants
    (
    -- * Paths
      pathToImages
    , pathToPages
    , pathToTemplates
    -- * Routes
    , pageRouteAlias
    , pageRouteDefault
    , pageRouteFromDataDirectory
    , pageRouteStatic
    -- * Duplication
    , duplicateField
    , duplicateMapField
    , duplicatedFields
    -- * Contexts
    , makeContext
    , siteContext
    , routeContext
    , fileContext
    -- * Templates
    , defaultTemplate
    , makeTemplate
    -- * Other
    , baseUrl
    , pageFinalizer
    , robustModificationTimeField
    , siteVersion
    , siteRepository
    ) where

import Control.Applicative            (Alternative(empty))
import Data.Foldable
import Data.List                      (isPrefixOf)
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
import System.FilePath.Posix ((</>), dropExtension, joinPath, splitDirectories, takeBaseName)


baseUrl :: String
baseUrl = "https://recursion.ninja"


pathToImages :: FilePath
pathToImages = "data" </> "img"


pathToPages :: FilePath
pathToPages = "data" </> "page"


pathToTemplates :: FilePath
pathToTemplates = "data" </> "mold"


siteVersion :: Version
siteVersion = version


siteRepository :: String
siteRepository = "https://github.com/recursion-ninja/personal-website"


pageRouteAlias :: FilePath -> String -> Routes
pageRouteAlias alias = composeRoutes (constRoute alias) . setExtension


pageRouteStatic :: String -> Routes
pageRouteStatic ex = customRoute (takeBaseName . toFilePath) `composeRoutes` setExtension ex


pageRouteDefault :: String -> Routes
pageRouteDefault = setExtension


pageRouteFromDataDirectory :: Routes
pageRouteFromDataDirectory = customRoute (fromDataDirectory . toFilePath)


fromDataDirectory :: FilePath -> FilePath
fromDataDirectory =
    let dropDataDirectory = \case
          []  -> []
          x:xs | "data" `isPrefixOf` x -> xs
          xs  -> xs
    in  joinPath . dropDataDirectory . splitDirectories
    

makeTemplate :: FilePath -> Identifier
makeTemplate = fromString . (pathToTemplates </>)


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
          err :: String
          err = fail $ "No route url found for item " <> show x
          f y = let url  = toUrl y
                    root = toSiteRoot url
                in  root </> dropExtension url
      maybe err f <$> getRoute x


fileContext :: Context String
fileContext =
    field "BaseFile" $ \i -> do
      let x = itemIdentifier i
          err :: String
          err = fail $ "No route url found for item " <> show x
      maybe err (takeBaseName . toUrl) <$> getRoute x


defaultTemplate :: Identifier
defaultTemplate = fromString $ pathToTemplates </> "default.html"


pageFinalizer
  :: Item String
  -> Compiler (Item String)
pageFinalizer x = relativizeUrls x >>= saveSnapshot "content"
