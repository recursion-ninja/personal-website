{-# Language OverloadedStrings #-}

module Indexer.Feed
    ( constructAtomFeed
    , constructRssFeed
    ) where

import Compiler.BlogPostContext
import Hakyll.Core.Compiler (Compiler, loadAllSnapshots)
import Hakyll.Core.Identifier (Identifier)
import Hakyll.Core.Identifier.Pattern (hasVersion, (.&&.))
import Hakyll.Core.Item (Item)
import Hakyll.Core.Routes (idRoute)
import Hakyll.Core.Rules (Rules, compile, create, route)
import Hakyll.Web.Feed (FeedConfiguration(..), renderAtom, renderRss)
import Hakyll.Web.Template.Context (Context)
import Hakyll.Web.Template.List (recentFirst)


myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Excursuses of Recursion Ninja"
    , feedDescription = "Shared Commentary on Selected Computational Topics"
    , feedAuthorName  = "Recursion Ninja"
    , feedAuthorEmail = "contact@recursion.ninja"
    , feedRoot        = "https://recursion.ninja"
    }


constructAtomFeed :: Rules ()
constructAtomFeed = constructFeed renderAtom "atom.xml"


constructRssFeed :: Rules ()
constructRssFeed = constructFeed renderRss "rss.xml"


constructFeed
    :: (FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String))
    -> Identifier
    -> Rules ()
constructFeed f s = do
    create [s] $ do
        route idRoute
        compile $ do
            blogPosts <- recentFirst =<< loadAllSnapshots ("blog/*" .&&. hasVersion "html") "content"
            f myFeedConfiguration blogPostContext blogPosts

