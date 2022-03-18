{-# LANGUAGE OverloadedStrings #-}

module Indexer.Page
    ( constructIndexPage
    ) where

import Compiler.Constants
import Hakyll


constructIndexPage :: Rules ()
constructIndexPage =
    let indexContext = makeContext
          [ constField "Redirect" "/about.html"
          , constField "Title"    "About"
          , constField "NavRef"   "about"
          ]
    in  create ["index.html"] $ do
          route idRoute
          compile $ makeItem mempty
              >>= loadAndApplyTemplate defaultTemplate indexContext
              >>= pageFinalizer
