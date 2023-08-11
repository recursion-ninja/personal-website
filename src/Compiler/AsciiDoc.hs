module Compiler.AsciiDoc (
    compileFormatAsciiDoc,
) where

import Compiler.Generic
import Data.Text (unpack)
import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Text.Pandoc.Writers


compileFormatAsciiDoc ∷ FormatCompiler
compileFormatAsciiDoc = formatCompilerFromSpecification "adoc" asciiDocCompiler


asciiDocCompiler ∷ Compiler (Item String)
asciiDocCompiler = fmap unpack <$> compilerFromWriter' "asciiDocCompiler" writeAsciiDoc
