{-# Language OverloadedStrings #-}

module Compiler.PDF
    ( -- * Compilers
      compileFormatPDF
    , compileFormatPDFWith
      -- * Markdown Transformations
    , blockFilter
    ) where

import Compiler.Generic
import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.STRef
import Data.Text (Text, unpack, words)
import Hakyll.Core.Compiler
import Hakyll.Core.File
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Item
import Hakyll.Core.Routes
import Hakyll.Core.Rules
import Hakyll.Web.Html.RelativizeUrls
import Hakyll.Web.Template
import Hakyll.Web.Template.Context
import Prelude hiding (words)
import System.FilePath.Posix
import System.Process (system)
import Text.Pandoc.Definition
import Text.Pandoc.Writers


useXeLaTeX :: Bool
useXeLaTeX = False


{- |
Default PDF format compiler.
-}
compileFormatPDF :: Context String -> Pattern -> (String -> Routes) -> [Identifier] -> Rules ()
compileFormatPDF = compileFormatPDFWith id "pdf"


{- |
Compile with an alteration.
-}
compileFormatPDFWith
    :: (Pandoc -> Pandoc) -- ^ Alteration Function
    -> String             -- ^ Alteration "Versioning Label"
    -> Context String
    -> Pattern
    -> (String -> Routes)

    -> [Identifier]
    -> Rules ()
compileFormatPDFWith f v inContext inPath inRoute inTemplates = match inPath . version v $ do
    route $ inRoute "pdf"
    compile $ texCompiler f >>= \x ->
        foldlM (\s t -> loadAndApplyTemplate t inContext s) x inTemplates
            >>= relativizeUrls
            >>= latex
            >>= saveSnapshot "content"


{- |
Filter out the specified headers.
-}
blockFilter :: (Foldable f, Functor f) => f (Int, Text) -> Pandoc -> Pandoc
blockFilter excludedHeaders (Pandoc meta blocks) = Pandoc meta $ fold blocks'
    where
        blocks' :: [[Block]]
        blocks' = runST $ do
            exclusionSet <- newSTRef excludedHeaders
            excluded     <- newSTRef Nothing
            let filterExcludes input = do
                    es <- readSTRef exclusionSet
                    ex <- readSTRef excluded
                    case ex of
                        Nothing  -> pure ()
                        Just val -> when (isSameBlockType val input) $ writeSTRef excluded Nothing
                    let (_, title) = getHeaderTitle input
                    when ((1, title) `elem` es) $ writeSTRef excluded (Just input)
                    modifySTRef exclusionSet
                        $ \x -> (\e@(n, ht) -> if ht == title then (n - 1, ht) else e) <$> x
                    ex' <- readSTRef excluded
                    pure $ maybe [input] (const []) ex'
            traverse filterExcludes blocks


clearSpaces :: Text -> Text
clearSpaces = fold . words


getHeaderTitle :: Block -> (Int, Text)
getHeaderTitle (Header n _ lineParts) = (n, foldMap inlineText lineParts)
getHeaderTitle _                      = (0, mempty)


inlineText :: Inline -> Text
inlineText (Str         txt) = clearSpaces txt
inlineText (Emph        ts ) = foldMap inlineText ts
inlineText (Underline   ts ) = foldMap inlineText ts
inlineText (Strong      ts ) = foldMap inlineText ts
inlineText (Strikeout   ts ) = foldMap inlineText ts
inlineText (Superscript ts ) = foldMap inlineText ts
inlineText (Subscript   ts ) = foldMap inlineText ts
inlineText (SmallCaps   ts ) = foldMap inlineText ts
inlineText (Quoted    _ ts ) = foldMap inlineText ts
inlineText (Cite      _ ts ) = foldMap inlineText ts
inlineText (Code      _ txt) = clearSpaces txt
inlineText (Math      _ txt) = clearSpaces txt
inlineText (RawInline _ txt) = clearSpaces txt
inlineText (Link  _ ts _   ) = foldMap inlineText ts
inlineText (Image _ ts _   ) = foldMap inlineText ts
inlineText (Span _ ts      ) = foldMap inlineText ts
inlineText _                 = ""


isSameBlockType :: Block -> Block -> Bool
isSameBlockType Plain{}          Plain{}          = True
isSameBlockType Para{}           Para{}           = True
isSameBlockType LineBlock{}      LineBlock{}      = True
isSameBlockType CodeBlock{}      CodeBlock{}      = True
isSameBlockType RawBlock{}       RawBlock{}       = True
isSameBlockType BlockQuote{}     BlockQuote{}     = True
isSameBlockType OrderedList{}    OrderedList{}    = True
isSameBlockType BulletList{}     BulletList{}     = True
isSameBlockType DefinitionList{} DefinitionList{} = True
isSameBlockType (Header x _ _)   (Header y _ _)   = x == y
isSameBlockType HorizontalRule   HorizontalRule   = True
isSameBlockType Table{}          Table{}          = True
isSameBlockType Div{}            Div{}            = True
isSameBlockType _                _                = False


latex :: Item String -> Compiler (Item TmpFile)
latex item = do
    TmpFile texPath <- newTmpFile "latex.tex"
    let tmpDir  = takeDirectory texPath
        pdfPath = replaceExtension texPath "pdf"

    _ <- unsafeCompiler $ do
        writeFile texPath $ itemBody item
        system $ unwords
            [ latexEngine
            , "--halt-on-error"
            , "-output-directory"
            , tmpDir
            , texPath
--            , ">/dev/null"
--            , "2>&1"
            ]

    makeItem $ TmpFile pdfPath


latexEngine :: String
latexEngine
    | useXeLaTeX = "xelatex"
    | otherwise  = "pdflatex"


texCompiler :: (Pandoc -> Pandoc) -> Compiler (Item String)
texCompiler f = fmap unpack <$> compilerFromWriter "latexCompiler" (\opt -> writeLaTeX opt . f)
