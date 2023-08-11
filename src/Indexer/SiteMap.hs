{-# LANGUAGE OverloadedStrings #-}

module Indexer.SiteMap (
    constructSiteMap,
) where

import Compiler.Constants
import Data.String
import Hakyll.Core.Compiler (loadAllSnapshots, makeItem)
import Hakyll.Core.Identifier.Pattern (hasVersion)
import Hakyll.Core.Routes (idRoute)
import Hakyll.Core.Rules (Rules, compile, create, route)
import Hakyll.Web.Template (loadAndApplyTemplate)
import Hakyll.Web.Template.Context (listField)
import System.FilePath.Posix ((</>))


constructSiteMap ∷ Rules ()
constructSiteMap =
    let siteMapTemplate = fromString $ pathToTemplates </> "sitemap.xml"
    in  create ["sitemap.xml"] $ do
            route idRoute
            compile $ do
                allMapPoints ← loadAllSnapshots (hasVersion "html") "content"
                let siteMapContext =
                        contextUsing [listField "SiteEntries" (contextUsing []) $ pure allMapPoints]
                makeItem "" >>= loadAndApplyTemplate siteMapTemplate siteMapContext
