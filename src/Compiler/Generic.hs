module Compiler.Generic where

import Compiler.Constants
import Data.Foldable
import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Item
import Hakyll.Core.Routes
import Hakyll.Core.Rules
import Hakyll.Web.Pandoc
import Hakyll.Web.Template
import Hakyll.Web.Template.Context
import Text.Pandoc.Class              (PandocPure, runPure)
import Text.Pandoc.Definition
import Text.Pandoc.Options


customCompiler
  :: Foldable t
  => String
  -> Compiler (Item String)
  -> Context String
  -> Pattern
  -> (String -> Routes)
  -> t Identifier
  -> Rules ()
customCompiler extension compiler inputContext inputPath inputRoute inputTemplates =
    match inputPath . version extension $ do
        route   $  inputRoute extension
        compile $ compiler
                >>= \x -> foldlM (\s t -> loadAndApplyTemplate t inputContext s) x inputTemplates
                >>= pageFinalizer


compilerFromWriter
  :: String
  -> (WriterOptions -> Pandoc -> PandocPure a)
  -> Compiler (Item a)
compilerFromWriter label writer = getResourceBody >>= readPandoc >>= withItemBody f
  where
    f x = case runPure $ writer defaultHakyllWriterOptions x of
            Left  err -> error $ label <> ": " <> show err
            Right bs  -> pure bs
