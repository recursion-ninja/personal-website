{-# Language LambdaCase #-}
{-# Language ScopedTypeVariables #-}

module Compiler.Generic
    ( -- * Format Compiler
      FormatCompiler
      -- ** Constructor
    , formatCompilerFromSpecification
      -- ** Base Compiler Builders
    , compilerFromWriter
    , compilerFromWriter'
      -- * Transformations
    , PandocTransform()
      -- ** Constructor
    , pandocTransform
      -- ** Accessor
    , getPandocTransform
      -- * Reader Options
    , defaultWebsiteReaderOptions
    ) where

import Compiler.Constants
import Compiler.Transformation.Table (equalizeTableColumns)
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


{- |
Strongly typed transformation of 'Pandoc' document resource.
-}
newtype PandocTransform = TransF (Pandoc -> Pandoc)


{- |
Construct a 'Pandoc' document transformation.
-}
pandocTransform :: (Pandoc -> Pandoc) -> PandocTransform
pandocTransform = TransF


{- |
Extract the 'Pandoc' document transformation.

Automatically applied default transformations in addition to the specified transformation.
-}
getPandocTransform :: PandocTransform -> (Pandoc -> Pandoc)
getPandocTransform (TransF f) = f . equalizeTableColumns


{- |
Construct a 'FormatCompiler' from the given specification.
-}
formatCompilerFromSpecification :: String -> Compiler (Item String) -> FormatCompiler
formatCompilerFromSpecification extension compiler inputContext inputPath inputRoute inputTemplates =
    let applyTemplates :: Item String -> Compiler (Item String)
        applyTemplates = flip (foldlM templateFold) inputTemplates

        -- Important, this needs to be a LEFT fold to apply templates in the correct order.
        templateFold :: Item String -> Identifier -> Compiler (Item String)
        templateFold s t = loadAndApplyTemplate t inputContext s

    in  match inputPath . version extension $ do
            route $ inputRoute extension
            compile $ compiler >>= applyTemplates >>= finalizePage


{- |
Construct a 'Compiler' from a "writer description" and a 'Pandoc' document transformation.

The resulting compiler reads the resource body(ies), applies the document transformtion,
then runs the writer description to produce output.
-}
compilerFromWriter :: forall a. String -> (WriterOptions -> Pandoc -> PandocPure a) -> PandocTransform -> Compiler (Item a)
compilerFromWriter label writer pTrans =
    let applyWriter :: Pandoc -> Either PandocError a
        applyWriter = runPure . writer defaultHakyllWriterOptions

        handleError :: Either PandocError a -> Compiler a
        handleError = \case
            Right val -> pure val
            Left  err -> error $ label <> ": " <> show err

        makeCompiler :: Pandoc -> Compiler a
        makeCompiler = handleError . applyWriter

    in  procurePandocResource pTrans >>= withItemBody makeCompiler


procurePandocResource :: PandocTransform -> Compiler (Item Pandoc)
procurePandocResource pTrans =
    let alterPandoc :: Compiler (Item Pandoc) -> Compiler (Item Pandoc)
        alterPandoc = fmap (fmap transformation)

        -- Always equalize table columns, then apply other tansformations
        transformation :: Pandoc -> Pandoc
        transformation = getPandocTransform pTrans

    in  getResourceBody >>= alterPandoc . readPandocWith defaultWebsiteReaderOptions


{- |
Construct a 'Compiler' from a "writer description" /without/ any 'Pandoc' document transformation.

See 'compilerFromWriter' for more information.
-}
compilerFromWriter' :: String -> (WriterOptions -> Pandoc -> PandocPure a) -> Compiler (Item a)
compilerFromWriter' = flip flip (pandocTransform id) . compilerFromWriter


{- |
Default reader options for personal website.

Enables extentions:
  * 'Ext_grid_tables'
  * 'Ext_tex_math_dollars'
-}
defaultWebsiteReaderOptions :: ReaderOptions
defaultWebsiteReaderOptions =
    let defaultOpts :: ReaderOptions
        defaultOpts = defaultHakyllReaderOptions
        defaultExts = readerExtensions def
        updatedExts = foldr enableExtension defaultExts [ Ext_grid_tables, Ext_tex_math_dollars ]
    in  defaultOpts { readerExtensions = updatedExts }
