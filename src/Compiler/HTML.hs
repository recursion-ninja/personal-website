module Compiler.HTML
    ( compileFormatHTML
    , compileFormatStaticHTML
    , compileFormatTransformedHTML
    ) where

import Compiler.Constants
import Compiler.Generic
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Rules
import Hakyll.Web.Pandoc
import Hakyll.Web.Template.Context
import Text.Pandoc.Definition


compileFormatHTML :: FormatCompiler
compileFormatHTML = compileFormatTransformedHTML id


compileFormatStaticHTML
  :: Context String
  -> Pattern
  -> [Identifier]
  -> Rules ()
compileFormatStaticHTML inputContext inputPath =
    compileFormatHTML inputContext inputPath pageRouteStatic


compileFormatTransformedHTML :: (Pandoc -> Pandoc) -> FormatCompiler
compileFormatTransformedHTML transform =
    let compiler = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions transform
    in  compilerFromSpecification "html" compiler
