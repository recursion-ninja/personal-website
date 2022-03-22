{-# Language OverloadedStrings #-}

module Indexer.SiteMap
    ( constructSiteMap
    ) where

import Compiler.Constants
import Data.String
import Hakyll
import System.FilePath.Posix ((</>))


constructSiteMap :: Rules ()
constructSiteMap =
    let siteMapTemplate = fromString $ pathToTemplates </> "sitemap.xml"
    in  create ["sitemap.xml"] $ do
            route idRoute
            compile $ do
                allMapPoints <- loadAllSnapshots (hasVersion "html") "content"
                let siteMapContext =
                        contextUsing [listField "SiteEntries" (contextUsing []) $ pure allMapPoints]
                makeItem "" >>= loadAndApplyTemplate siteMapTemplate siteMapContext
