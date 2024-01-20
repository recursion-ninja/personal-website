{-# LANGUAGE OverloadedStrings #-}

module Compiler.PDF (
    -- * Compilers
    compileFormatPDF,
    compileFormatPDFWith,
) where

import Compiler.Generic
import Compiler.Transformation.Table (equalizeTableColumns)
import Control.Monad
import Data.Foldable
import Data.Text (Text, unpack)
import Hakyll.Core.Compiler
import Hakyll.Core.File
import Hakyll.Core.Identifier
import Hakyll.Core.Item (Item, itemBody)
import Hakyll.Core.Rules
import Hakyll.Web.Html.RelativizeUrls
import Hakyll.Web.Template
import System.FilePath.Posix
import System.Process (system)
import Text.Pandoc.Definition
import Text.Pandoc.Writers
import Prelude hiding (words)


useXeLaTeX ∷ Bool
useXeLaTeX = False


{- |
Default PDF format compiler.
-}
compileFormatPDF ∷ FormatCompiler
compileFormatPDF = compileFormatPDFWith id "pdf"


{- |
Compile with an alteration.
-}
compileFormatPDFWith
    ∷ (Pandoc → Pandoc)
    -- ^ Alteration Function
    → String
    -- ^ Alteration "Versioning Label"
    → FormatCompiler
compileFormatPDFWith f v inContext inPath inRoute inTemplates =
    let -- Always equalize table columns, then apply other transformations
        transformation ∷ Pandoc → Pandoc
        transformation = f . equalizeTableColumns

        templateFold ∷ Item String → Identifier → Compiler (Item String)
        templateFold s t = loadAndApplyTemplate t inContext s
    in  match inPath . version v $ do
            route $ inRoute "pdf"
            compile $
                texCompiler transformation >>= \x →
                    foldlM templateFold x inTemplates
                        >>= relativizeUrls
                        >>= latex
                        >>= saveSnapshot "content"


latex ∷ Item String → Compiler (Item TmpFile)
latex item = do
    TmpFile texPath ← newTmpFile "latex.tex"
    let tmpDir = takeDirectory texPath
        pdfPath = replaceExtension texPath "pdf"

    _ ← unsafeCompiler $ do
        writeFile texPath $ itemBody item
        system $
            unwords
                [ latexEngine
                , "--halt-on-error"
                , "-output-directory"
                , tmpDir
                , texPath
                --            , ">/dev/null"
                --            , "2>&1"
                ]

    makeItem $ TmpFile pdfPath


latexEngine ∷ String
latexEngine
    | useXeLaTeX = "xelatex"
    | otherwise = "pdflatex"


texCompiler ∷ (Pandoc → Pandoc) → Compiler (Item String)
texCompiler =
    let makeCompiler ∷ PandocTransform → Compiler (Item Text)
        makeCompiler = compilerFromWriter "latexCompiler" writeLaTeX

        unpackStream ∷ Compiler (Item Text) → Compiler (Item String)
        unpackStream = fmap (fmap unpack)
    in  unpackStream . makeCompiler . pandocTransform
