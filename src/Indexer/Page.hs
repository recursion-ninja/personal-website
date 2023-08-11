{-# LANGUAGE OverloadedStrings #-}

module Indexer.Page (
    constructIndexHTML,
) where

import Compiler.Constants
import Hakyll.Core.Compiler (makeItem)
import Hakyll.Core.Routes (idRoute)
import Hakyll.Core.Rules (Rules, compile, create, route)
import Hakyll.Web.Template (loadAndApplyTemplate)
import Hakyll.Web.Template.Context (constField)


constructIndexHTML ∷ Rules ()
constructIndexHTML =
    let indexContext =
            contextUsing
                [constField "Redirect" "/about.html", constField "Title" "About", constField "NavRef" "about"]
    in  create ["index.html"] $ do
            route idRoute
            compile $ makeItem mempty >>= loadAndApplyTemplate templateDefault indexContext >>= finalizePage
