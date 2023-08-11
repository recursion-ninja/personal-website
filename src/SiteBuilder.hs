module Main (
    main,
) where

import AssetIncludes
import Data.Foldable
import Hakyll.Core.Rules (Rules)
import Hakyll.Main (hakyll)
import Indexer.Feed
import Indexer.HypertextAccess
import Indexer.Page
import Indexer.Robots
import Indexer.SiteMap
import PageBuilder.About
import PageBuilder.Blog
import PageBuilder.CV
import PageBuilder.Errors


main ∷ IO ()
main = hakyll $ do
    compileAllAssets
    compileAllPages
    compileAllIndices


{- |
  Include all assets required by website pages.
-}
compileAllAssets ∷ Rules ()
compileAllAssets = sequenceA_ [includeCSS, includeImages, includeFavicons, includeTemplates]


{- |
  Build all website pages.
-
-}
compileAllPages ∷ Rules ()
compileAllPages =
    sequenceA_
        [buildAbout, buildBlogList, buildBlogPosts, buildCV, buildError400, buildError404, buildError500]


{- |
  Construct all indices of website.

  __NOTE:__ This must occur /after/ assets and webpages have been compiled!
-}
compileAllIndices ∷ Rules ()
compileAllIndices =
    sequenceA_
        [ constructAtomFeed
        , constructIndexHTML
        , constructHypertextAccess
        , constructRssFeed
        , constructRobotsTXT
        , constructSiteMap
        ]
