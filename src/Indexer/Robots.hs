{-# LANGUAGE OverloadedStrings #-}

module Indexer.Robots
    ( constructRobotsTXT
    ) where

import Hakyll


constructRobotsTXT :: Rules ()
constructRobotsTXT =
    create ["robots.txt"] $ do
      route idRoute
      compile $ makeItem textPayload
  where
    textPayload :: String
    textPayload = unlines
        [ "User-agent: *"
        , "Disallow:"
        ]
