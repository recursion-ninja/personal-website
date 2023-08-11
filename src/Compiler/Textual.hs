module Compiler.Textual (
    compileFormatTextual,
) where

import Compiler.Generic
import Data.Text (unpack)
import Hakyll.Core.Compiler (Compiler)
import Hakyll.Core.Item (Item)
import Text.Pandoc.Writers (writePlain)


compileFormatTextual ∷ FormatCompiler
compileFormatTextual = formatCompilerFromSpecification "txt" textualCompiler


textualCompiler ∷ Compiler (Item String)
textualCompiler = fmap unpack <$> compilerFromWriter' "textualCompiler" writePlain
