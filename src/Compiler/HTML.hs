module Compiler.HTML (
    compileFormatHTML,
    compileFormatStaticHTML,
    compileFormatTransformedHTML,
) where

import Compiler.Constants
import Compiler.Generic
-- import Data.Text (unpack)
import Hakyll.Core.Compiler (Compiler)
import Hakyll.Core.Identifier (Identifier)
import Hakyll.Core.Identifier.Pattern (Pattern)
import Hakyll.Core.Item (Item)
import Hakyll.Core.Rules (Rules)
import Hakyll.Web.Pandoc
import Hakyll.Web.Template.Context (Context)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions, WriterOptions)


-- import Text.Pandoc.Writers (writeHtml5String)

compileFormatHTML ∷ FormatCompiler
compileFormatHTML = compileFormatTransformedHTML id


compileFormatStaticHTML ∷ Context String → Pattern → [Identifier] → Rules ()
compileFormatStaticHTML inputContext inputPath = compileFormatHTML inputContext inputPath pageRouteStatic


compileFormatTransformedHTML ∷ (Pandoc → Pandoc) → FormatCompiler
compileFormatTransformedHTML =
    let htmlCompiler ∷ (Pandoc → Pandoc) → Compiler (Item String)
        htmlCompiler = pandocCompilerWithTransform optsR optsW

        htmlTranstion ∷ (Pandoc → Pandoc) → (Pandoc → Pandoc)
        htmlTranstion = getPandocTransform . pandocTransform

        optsR ∷ ReaderOptions
        optsR = defaultWebsiteReaderOptions

        optsW ∷ WriterOptions
        optsW = defaultHakyllWriterOptions
    in  formatCompilerFromSpecification "html" . htmlCompiler . htmlTranstion
