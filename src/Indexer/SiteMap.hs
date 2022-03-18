{-# LANGUAGE OverloadedStrings #-}

module Indexer.SiteMap
    ( constructSiteMap
    ) where

import Compiler.Constants
import Data.String
import Hakyll
import System.FilePath.Posix ((</>))


constructSiteMap :: Rules ()
constructSiteMap =
    let siteMapTemplate = fromString $ templatePath </> "sitemap.xml"
    in  create ["sitemap.xml"] $ do
          route idRoute
          compile $ do
            allMapPoints <- loadAllSnapshots (hasVersion "html") "content"
            let siteMapContext = makeContext
                    [ listField "SiteEntries" (makeContext []) $ pure allMapPoints
                    ]
            makeItem "" >>= loadAndApplyTemplate siteMapTemplate siteMapContext
