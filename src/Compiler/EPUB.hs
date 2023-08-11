module Compiler.EPUB (
    compileFormatEPUB,
) where

import Compiler.Generic
import Hakyll.Core.Rules
import Text.Pandoc.Writers


compileFormatEPUB ∷ FormatCompiler
compileFormatEPUB _ inputPath inputRoute _ = match inputPath . version "epub" $ do
    route $ inputRoute "epub"
    compile $ compilerFromWriter' "epubCompiler" writeEPUB3
