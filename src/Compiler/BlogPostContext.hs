{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler.BlogPostContext (
    blogPostContext,
    blogPostMetadataFields,
    blogPostPath,
) where

import Compiler.Constants
import Crypto.Hash (Digest, SHA3_512 (SHA3_512), hashWith)
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Hakyll.Core.Compiler (getResourceString, makeItem, noResult)
import Hakyll.Core.Identifier.Pattern (Pattern)
import Hakyll.Core.Item (itemBody)
import Hakyll.Web.Pandoc (defaultHakyllWriterOptions, readPandoc)
import Hakyll.Web.Template.Context
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition
import Text.Pandoc.Writers


blogPostPath ∷ Pattern
blogPostPath = "blog/*.md"


blogPostContext ∷ Context String
blogPostContext =
    contextUsing
        [ blogPostMetadataFields
        , blogPostDescription
        , constField "NavRef" "blog"
        , dateField "Date" "1%0Y+%j %A, %B %e"
        , flagField "IsBlogPost"
        , flagField "HasDownloadFormats"
        , flagField "HasASCIIDoc"
        , flagField "HasEPUB"
        , flagField "HasPDF"
        , flagField "HasMarkdown"
        , flagField "HasTextual"
        ]


blogPostDescription ∷ Context a
blogPostDescription = "Subtitle" `contextAliasFieldAs` "description"


blogPostMetadataFields ∷ Context a
blogPostMetadataFields = Context $ \k _ _ → do
    raw ← getResourceString
    doc ← readPandoc raw
    let (wc, loc, hash) = collateBlogPostMetadata $ itemBody doc
    let hashValue = toUpper <$> show hash
    let hashRows = 8
    let hashTokens = length hashValue
    let rowLen = hashTokens `div` hashRows
    let str = pure . StringField . show
    let hashOctile i = take rowLen $ drop (i * rowLen) hashValue
    case k of
        "Hash" → pure $ StringField hashValue
        "HashOctiles" → ListField octileContext <$> traverse (makeItem . hashOctile) [0 .. hashRows - 1]
        "HashOctile0" → fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 0
        "HashOctile1" → fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 1
        "HashOctile2" → fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 2
        "HashOctile3" → fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 3
        "HashOctile4" → fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 4
        "HashOctile5" → fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 5
        "HashOctile6" → fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 6
        "HashOctile7" → fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 7
        "WordCount" → str wc
        "LOC" → str loc
        "ERT" →
            let -- Note the estimated 265 words per minute
                -- 226ms ~ 60s / 265
                msWords = wc * 226
                -- We estimate 6.5s per line of code
                msLOC = loc * 6500
                (m, r) = (msWords + msLOC) `quotRem` 60000
                reqMins = m + if r == 0 then 0 else 1
            in  str reqMins
        _ → noResult $ "Tried field " <> k
    where
        octileContext = Context $ \k _ i → do
            StringField body ← unContext defaultContext "body" [] i
            case k of
                "HexChars" → ListField defaultContext <$> traverse (makeItem . pure) body
                _ → noResult $ "Tried field " <> k


{- |
An accumulator type to collect relevant blog post metrics in a single pass.
-}
newtype PandocDigestion = A {vomitContents ∷ (Word, Word, Text, ByteString)}


instance Semigroup PandocDigestion where
    (A (w, x, y, z)) <> (A (a, b, c, d)) = A (w + a, x + b, y <> c, z <> d)


instance Monoid PandocDigestion where
    mempty = A (0, 0, mempty, mempty)


codeAccum, wordAccum ∷ Text → PandocDigestion
codeAccum x = let ls = T.lines x in A (0, toEnum $ length ls, x, T.encodeUtf8 $ T.unlines ls)
wordAccum x = let ws = T.words x in A (toEnum $ length ws, 0, mempty, T.encodeUtf8 $ T.unwords ws)


{- |
Returns the:
  - Word count
  - Lines of code
  - SHA3-512 hash
-}
collateBlogPostMetadata ∷ Pandoc → (Word, Word, Digest SHA3_512)
collateBlogPostMetadata doc@(Pandoc _ blocks) = (wordCount, codeCount, hashDigest)
    where
        (wordCount, codeCount, _, _) = vomitContents $ eatBlocks blocks

        renderTxtDoc = runPure . writePlain defaultHakyllWriterOptions
        tokenizeText = T.unwords . T.words . T.intercalate " " . T.lines
        hashDigest = case renderTxtDoc doc of
            Left _ → error "Could not serialize data for hashing"
            Right txt → hashWith SHA3_512 . T.encodeUtf8 $ tokenizeText txt

        singleWord = A (1, 0, mempty, mempty)

        eatBody (TableBody _ _ rows1 rows2) = foldMap eatRow (rows1 <> rows2)
        eatCell (Cell _ _ _ _ bPart) = eatBlocks bPart
        eatFoot (TableFoot _ rows) = foldMap eatRow rows
        eatHead (TableHead _ rows) = foldMap eatRow rows
        eatRow (Row _ cells) = foldMap eatCell cells

        eatBlock ∷ Block → PandocDigestion
        eatBlock (Table _ _ _ tHead tBody tFoot) = fold [eatHead tHead, foldMap eatBody tBody, eatFoot tFoot]
        eatBlock HorizontalRule = mempty
        eatBlock (CodeBlock _ txt) = codeAccum txt
        eatBlock (RawBlock _ txt) = wordAccum txt
        eatBlock (Plain iPart) = eatLines iPart
        eatBlock (Para iPart) = eatLines iPart
        eatBlock (Header _ _ iPart) = eatLines iPart
        eatBlock (BlockQuote bPart) = eatBlocks bPart
        eatBlock (Div _ bPart) = eatBlocks bPart
        eatBlock (Figure _ _ bParts) = eatBlocks bParts
        eatBlock (OrderedList _ bParts) = foldMap eatBlocks bParts
        eatBlock (BulletList bParts) = foldMap eatBlocks bParts
        eatBlock (LineBlock iParts) = foldMap eatLines iParts
        eatBlock (DefinitionList list) =
            let f ∷ (Foldable t) ⇒ ([Inline], t [Block]) → PandocDigestion
                f (iPart, bParts) = eatLines iPart <> foldMap eatBlocks bParts
            in  foldMap f list

        eatBlocks ∷ [Block] → PandocDigestion
        eatBlocks = foldMap eatBlock

        eatLine ∷ Inline → PandocDigestion
        eatLine Space = singleWord
        eatLine SoftBreak = singleWord
        eatLine LineBreak = singleWord
        eatLine (Note bPart) = eatBlocks bPart
        eatLine (Emph iPart) = eatLines iPart
        eatLine (Underline iPart) = eatLines iPart
        eatLine (Strong iPart) = eatLines iPart
        eatLine (Strikeout iPart) = eatLines iPart
        eatLine (Superscript iPart) = eatLines iPart
        eatLine (Subscript iPart) = eatLines iPart
        eatLine (SmallCaps iPart) = eatLines iPart
        eatLine (Span _ iPart) = eatLines iPart
        eatLine (Quoted _ iPart) = eatLines iPart
        eatLine (Cite _ iPart) = eatLines iPart
        eatLine (Code _ txt) = wordAccum txt
        eatLine (Math _ txt) = wordAccum txt
        eatLine (RawInline _ txt) = wordAccum txt
        eatLine (Str txt) = wordAccum txt
        eatLine (Link _ iPart (_, s)) = eatLines iPart <> wordAccum s
        eatLine (Image _ iPart (_, s)) = eatLines iPart <> wordAccum s

        eatLines ∷ [Inline] → PandocDigestion
        eatLines = foldMap eatLine
