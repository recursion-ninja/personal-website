{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compiler.Constants
import Data.Foldable
import Data.String
import Hakyll
import Indexer.Feed
import Indexer.Page
import Indexer.HypertextAccess
import Indexer.Robots
import Indexer.SiteMap
import PageBuilder.About
import PageBuilder.Blog
import PageBuilder.CV
import PageBuilder.Errors
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
    , error400PageBuilder
    , error404PageBuilder
    , error500PageBuilder
    ]


compileAllIndices :: Rules ()
compileAllIndices = sequenceA_
    [ constructAtomFeed
    , constructIndexPage
    , constructHypertextAccess
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
