{-# Language OverloadedStrings #-}

module Indexer.Robots
    ( constructRobotsTXT
    ) where

import Hakyll


constructRobotsTXT :: Rules ()
constructRobotsTXT =
    let textPayload = unlines ["User-agent: *", "Disallow:"] :: String
    in  create ["robots.txt"] $ do
            route idRoute
            compile $ makeItem textPayload

