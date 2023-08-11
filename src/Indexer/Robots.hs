{-# LANGUAGE OverloadedStrings #-}

module Indexer.Robots (
    constructRobotsTXT,
) where

import Hakyll.Core.Compiler (makeItem)
import Hakyll.Core.Routes (idRoute)
import Hakyll.Core.Rules (Rules, compile, create, route)


constructRobotsTXT ∷ Rules ()
constructRobotsTXT =
    let textPayload = unlines ["User-agent: *", "Disallow:"] ∷ String
    in  create ["robots.txt"] $ do
            route idRoute
            compile $ makeItem textPayload
