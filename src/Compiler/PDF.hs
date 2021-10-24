{-# LANGUAGE OverloadedStrings #-}

module Compiler.PDF
  ( compilePdfFormat
  , compilePdfFormatWith
  , blockFilter
  ) where

import Compiler.Generic
import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.STRef
import Data.Text                      (Text, unpack, words)
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
import Prelude                        hiding (words)
import System.FilePath.Posix
import System.Process                 (system)
import Text.Pandoc.Definition
import Text.Pandoc.Writers


compilePdfFormat
  :: Context String
  -> Pattern
  -> (String -> Routes)
  -> [Identifier]
  -> Rules ()
compilePdfFormat = compilePdfFormatWith id "pdf"


-- |
-- Compile with an alteration.
compilePdfFormatWith
  :: (Pandoc -> Pandoc) -- ^ Alteration Function
  -> String             -- ^ Alteration "Versioning Label"
  -> Context String
  -> Pattern
  -> (String -> Routes)
  -> [Identifier]
  -> Rules ()
compilePdfFormatWith f v inputContext inputPath inputRoute inputTemplates =
    match inputPath . version v $ do
        route   $  inputRoute "pdf"
        compile $ texCompiler f
                >>= \x -> foldlM (\s t -> loadAndApplyTemplate t inputContext s) x inputTemplates
                >>= relativizeUrls
                >>= latex
                >>= saveSnapshot "content"


latex :: Item String -> Compiler (Item TmpFile)
latex item = do
    TmpFile texPath <- newTmpFile "latex.tex"
    let tmpDir  = takeDirectory texPath
        pdfPath = replaceExtension texPath "pdf"

    _  <- unsafeCompiler $ do
            writeFile texPath $ itemBody item
            system $ unwords
              [ "pdflatex"
              , "--halt-on-error"
              , "-output-directory"
              , tmpDir
              , texPath
              ]

    makeItem $ TmpFile pdfPath


texCompiler :: (Pandoc -> Pandoc) -> Compiler (Item String)
texCompiler f = fmap unpack <$> compilerFromWriter "latexCompiler" (\opt -> writeLaTeX opt . f)


blockFilter :: (Foldable f, Functor f) => f (Int, Text) -> Pandoc -> Pandoc
blockFilter excludedHeaderings (Pandoc meta blocks) = Pandoc meta $ fold blocks'
  where
    blocks' :: [[Block]]
    blocks' = runST $ do
      exclusionSet <- newSTRef excludedHeaderings
      excluded     <- newSTRef Nothing
      let filterExcludes input = do
              es <- readSTRef exclusionSet
              ex <- readSTRef excluded
              case ex of
                Nothing  -> pure ()
                Just val ->
                  when (isSameBlockType val input) $
                    writeSTRef excluded Nothing
              let (_,title) = getHeaderTitle input
              when ((1,title) `elem` es) $
                writeSTRef excluded (Just input)
              modifySTRef exclusionSet $
                \x -> (\e@(n,ht) -> if ht == title then (n-1,ht) else e) <$> x
              ex' <- readSTRef excluded
              pure $ maybe [input] (const []) ex'
      traverse filterExcludes blocks


isSameBlockType :: Block -> Block -> Bool
isSameBlockType  Plain          {} Plain          {} = True
isSameBlockType  Para           {} Para           {} = True
isSameBlockType  LineBlock      {} LineBlock      {} = True
isSameBlockType  CodeBlock      {} CodeBlock      {} = True
isSameBlockType  RawBlock       {} RawBlock       {} = True
isSameBlockType  BlockQuote     {} BlockQuote     {} = True
isSameBlockType  OrderedList    {} OrderedList    {} = True
isSameBlockType  BulletList     {} BulletList     {} = True
isSameBlockType  DefinitionList {} DefinitionList {} = True
isSameBlockType (Header x _ _)    (Header y _ _)     = x == y
isSameBlockType  HorizontalRule    HorizontalRule    = True
isSameBlockType  Table          {} Table          {} = True
isSameBlockType  Div            {} Div            {} = True
isSameBlockType  Null              Null              = True
isSameBlockType  _                 _                 = False


getHeaderTitle :: Block -> (Int, Text)
getHeaderTitle (Header n _ inlines) = (n, foldMap inlineText inlines)
getHeaderTitle _                    = (0,"")


inlineText :: Inline -> Text
inlineText (Str         txt) = unspace txt
inlineText (Emph         ts) = foldMap inlineText ts
inlineText (Underline    ts) = foldMap inlineText ts
inlineText (Strong       ts) = foldMap inlineText ts
inlineText (Strikeout    ts) = foldMap inlineText ts
inlineText (Superscript  ts) = foldMap inlineText ts
inlineText (Subscript    ts) = foldMap inlineText ts
inlineText (SmallCaps    ts) = foldMap inlineText ts
inlineText (Quoted    _  ts) = foldMap inlineText ts
inlineText (Cite      _  ts) = foldMap inlineText ts
inlineText (Code      _ txt) = unspace txt
inlineText (Math      _ txt) = unspace txt
inlineText (RawInline _ txt) = unspace txt
inlineText (Link     _ ts _) = foldMap inlineText ts
inlineText (Image    _ ts _) = foldMap inlineText ts
inlineText (Span     _ ts  ) = foldMap inlineText ts
inlineText _                 = ""


unspace :: Text -> Text
unspace = fold . words
