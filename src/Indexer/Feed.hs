{-# LANGUAGE OverloadedStrings #-}

module Indexer.Feed
  ( constructAtomFeed
  , constructRssFeed
  ) where

import Compiler.BlogPostContext
import Hakyll


myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Excursuses of Recursion Ninja"
    , feedDescription = "Shared Commentary on Selected Computational Topics"
    , feedAuthorName  = "Recursion Ninja"
    , feedAuthorEmail = "contact@recursion.ninja"
    , feedRoot        = "http://recursion.ninja"
    }


constructAtomFeed :: Rules ()
constructAtomFeed = constructFeed renderAtom "atom.xml"


constructRssFeed  :: Rules ()
constructRssFeed  = constructFeed renderRss  "rss.xml"


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


