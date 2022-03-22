{-# Language LambdaCase #-}

module Compiler.Constants
    ( -- * Compiler
      finalizePage
      -- * Contexts
    , contextAliasFieldAs
    , contextUsing
      -- * Fields
    , flagField
      -- * Paths
    , pathToCSS
    , pathToFavicons
    , pathToImages
    , pathToPages
    , pathToTemplates
      -- * Routes
    , pageRouteAlias
    , pageRouteDefault
    , pageRouteFromDataDirectory
    , pageRouteStatic
      -- * Templates
    , templateDefault
    , templateUsing
    ) where

import Control.Applicative (Alternative(empty))
import Data.Foldable
import Data.List (isPrefixOf)
import Data.String
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
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
import Paths_personal_website (version)
import System.FilePath.Posix (dropExtension, joinPath, splitDirectories, takeBaseName, (</>))


finalizePage :: Item String -> Compiler (Item String)
finalizePage x = relativizeUrls x >>= saveSnapshot "content"


contextAliasFieldAs :: String -> String -> Context a
contextAliasFieldAs original clone = duplicateMapField original clone id


contextUsing :: Foldable f => f (Context String) -> Context String
contextUsing cs = duplicatedFields <> fold cs <> contextOfWebsite


flagField :: String -> Context a
flagField = flip boolField $ const True


pathToCSS :: FilePath
pathToCSS = pathFromData assetReferenceToCSS


pathToFavicons :: FilePath
pathToFavicons = pathFromData assetReferenceToFavicons


pathToImages :: FilePath
pathToImages = pathFromData assetReferenceToImages


pathToPages :: FilePath
pathToPages = pathFromData "page"


pathToTemplates :: FilePath
pathToTemplates = pathFromData "template"


pageRouteAlias :: FilePath -> String -> Routes
pageRouteAlias alias = composeRoutes (constRoute alias) . setExtension


pageRouteDefault :: String -> Routes
pageRouteDefault = setExtension


pageRouteFromDataDirectory :: Routes
pageRouteFromDataDirectory = customRoute (fromDataDirectory . toFilePath)


pageRouteStatic :: String -> Routes
pageRouteStatic ex = customRoute (takeBaseName . toFilePath) `composeRoutes` setExtension ex


{- |
  Default HTML template.
-}
templateDefault :: Identifier
templateDefault = fromString $ pathToTemplates </> "default.html"


{- |
  Use the specified template located within 'pathToTemplates'.


==== __Examples__


>>> templateUsing "blog-list.html"
-}
templateUsing :: FilePath -> Identifier
templateUsing = fromString . (pathToTemplates </>)


{-
-- Not Exported
-}


assetReferenceToCSS :: FilePath
assetReferenceToCSS = "css"


assetReferenceToFavicons :: FilePath
assetReferenceToFavicons = "fav"


assetReferenceToImages :: FilePath
assetReferenceToImages = "img"


contextOfFile :: Context String
contextOfFile = field "BaseFile" $ \i ->
    let x   = itemIdentifier i
        err = fail $ "No route url found for item " <> show x :: String
    in  maybe err (takeBaseName . toUrl) <$> getRoute x


contextOfRoute :: Context String
contextOfRoute = field "BaseRoute" $ \i -> do
    let x   = itemIdentifier i
        err = fail $ "No route url found for item " <> show x :: String
        f y =
            let url  = toUrl y
                root = toSiteRoot url
            in  root </> dropExtension url
    maybe err f <$> getRoute x


contextOfWebsite :: Context String
contextOfWebsite = fold
    [ constField "Author"            "Alex Washburn"
    , constField "BaseURL"           websiteURL
    , constField "rights"            "Creative Commons Non-Commercial Share Alike 3.0"
    , constField "language"          "en-US"
    , constField "WebsiteRepository" websiteRepository
    , constField "WebsiteVersion" $ showVersion websiteVersion
    , constField "AssetRefCSS"      assetReferenceToCSS
    , constField "AssetRefFavicons" assetReferenceToFavicons
    , constField "AssetRefImages"   assetReferenceToImages
    , constField "FilepathImages"   pathToImages
    , robustModificationTimeField "LastModified" "%B %e %0Y"
    , robustModificationTimeField "updated"      "%Y-%m-%dT%H:%M:%SZ"
    , contextOfRoute
    , contextOfFile
    , defaultContext
    ]


duplicateMapField :: String -> String -> (String -> String) -> Context a
duplicateMapField original clone f =
    field clone $ \i -> maybe empty f . lookupString original <$> getMetadata (itemIdentifier i)


duplicatedFields :: Context String
duplicatedFields = fold
    [ "Author" `contextAliasFieldAs` "name"
    , "Title" `contextAliasFieldAs` "title"
    , "Date" `contextAliasFieldAs` "date"
    ]


fromDataDirectory :: FilePath -> FilePath
fromDataDirectory =
    let dropDataDirectory = \case
            []                             -> []
            x : xs | "data" `isPrefixOf` x -> xs
            xs                             -> xs
    in  joinPath . dropDataDirectory . splitDirectories


pathFromData :: FilePath -> FilePath
pathFromData = ("data" </>)


robustModificationTimeField :: String -> String -> Context String
robustModificationTimeField key fmt = field key $ \i -> do
    let identifier = itemIdentifier i
    provider <- compilerProvider <$> compilerAsk
    modTime  <- if resourceExists provider identifier
        then pure $ resourceModificationTime provider identifier
        else unsafeCompiler getCurrentTime
    pure $ formatTime defaultTimeLocale fmt modTime


websiteRepository :: String
websiteRepository = "https://github.com/recursion-ninja/personal-website"


websiteURL :: String
websiteURL = "https://recursion.ninja"


websiteVersion :: Version
websiteVersion = version

