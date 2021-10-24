{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compiler.Constants
import Data.Foldable
import Data.String
import Hakyll
import Indexer.Feed
import Indexer.Page
import Indexer.Robots
import Indexer.SiteMap
import PageBuilder.About
import PageBuilder.Blog
import PageBuilder.CV
import PageBuilder.NotFound
import System.FilePath.Posix ((</>))


main :: IO ()
main = hakyll $ do
    includeAllResources
    compileAllSitePages
    compileAllIndices


compileAllSitePages :: Rules ()
compileAllSitePages = sequenceA_
    [    aboutPageBuilder
    ,     blogListBuilder
    ,     blogPostBuilder
    ,       cvPageBuilder
    , notFoundPageBuilder
    ]


compileAllIndices :: Rules ()
compileAllIndices = sequenceA_
    [ constructIndexPage
    , constructAtomFeed
    , constructRssFeed
    , constructRobotsTXT
    , constructSiteMap
    ]


includeAllResources :: Rules ()
includeAllResources = do
    match allTemplates $ compile templateBodyCompiler

    match "fav/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "img/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler


allTemplates :: Pattern
allTemplates = fromString $ templatePath </> "*"
