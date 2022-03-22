{-# Language OverloadedStrings #-}

module Indexer.Page
    ( constructIndexHTML
    ) where

import Compiler.Constants
import Hakyll


constructIndexHTML :: Rules ()
constructIndexHTML =
    let indexContext = contextUsing
            [constField "Redirect" "/about.html", constField "Title" "About", constField "NavRef" "about"]
    in  create ["index.html"] $ do
            route idRoute
            compile $ makeItem mempty >>= loadAndApplyTemplate templateDefault indexContext >>= finalizePage
