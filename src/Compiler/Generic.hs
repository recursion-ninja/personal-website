{-# Language LambdaCase #-}

module Compiler.Generic
    ( FormatCompiler
    , compilerFromSpecification
    , compilerFromWriter
    ) where

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
import Text.Pandoc.Class (PandocPure, runPure)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Options


{- |
  Type synonym for compiling alternative blog post formats.
-}
type FormatCompiler = Context String -> Pattern -> (String -> Routes) -> [Identifier] -> Rules ()


compilerFromSpecification
    :: Foldable t
    => String
    -> Compiler (Item String)
    -> Context String
    -> Pattern
    -> (String -> Routes)
    -> t Identifier
    -> Rules ()
compilerFromSpecification extension compiler inputContext inputPath inputRoute inputTemplates =
    let applyTemplates = flip (foldlM (\s t -> loadAndApplyTemplate t inputContext s)) inputTemplates
    in  match inputPath . version extension $ do
            route $ inputRoute extension
            compile $ compiler >>= applyTemplates >>= finalizePage


compilerFromWriter :: String -> (WriterOptions -> Pandoc -> PandocPure a) -> Compiler (Item a)
compilerFromWriter label writer =
    let applyWriter = runPure . writer defaultHakyllWriterOptions
        handleError :: Either PandocError a -> Compiler a
        handleError = \case
            Left  err -> error $ label <> ": " <> show err
            Right val -> pure val
        makeCompiler = handleError . applyWriter
    in  getResourceBody >>= readPandoc >>= withItemBody makeCompiler
