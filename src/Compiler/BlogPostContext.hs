{-# LANGUAGE OverloadedStrings #-}

module Compiler.BlogPostContext
  ( blogPostContext
  , blogPostMetadataFields
  , blogPostPath
  ) where

import           Compiler.Constants
import           Crypto.Hash            (Digest, SHA3_512(SHA3_512), hashWith)
import           Data.ByteString        (ByteString)
import           Data.Char              (toUpper)
import           Data.Foldable
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Hakyll
import           Text.Pandoc.Class      (runPure)
import           Text.Pandoc.Definition
import           Text.Pandoc.Writers


blogPostPath :: Pattern
blogPostPath = "blog/*"


blogPostContext :: Context String
blogPostContext = makeContext
    [ blogPostMetadataFields
    , blogPostDecription
    ,  dateField "Date"   "1%0Y+%j %A, %B %e"
    , constField "NavRef" "blog"
    ,  boolField "IsBlogPost"         $ const True
    ,  boolField "HasDownloadFormats" $ const True
    ,  boolField "HasASCIIDoc"        $ const True
    ,  boolField "HasEPUB"            $ const True
    ,  boolField "HasPDF"             $ const True
    ,  boolField "HasMarkdown"        $ const True
    ,  boolField "HasTextual"         $ const True
    ]


blogPostDecription :: Context a
blogPostDecription = duplicateField "Subtitle" "description"


blogPostMetadataFields :: Context a
blogPostMetadataFields = Context $ \k _ _ -> do
    raw <- getResourceString
    doc <- readPandoc raw
    let (wc, loc, hash) = collateBlogPostMetadata $ itemBody doc
    let hashVal  = toUpper <$> show hash
    let hashRows = 8
    let hashToks = length hashVal
    let rowLen   = hashToks `div` hashRows
    let str      = pure . StringField . show
    let hashOctile i = take rowLen $ drop (i*rowLen) hashVal
    case k of
      "Hash"        -> pure $ StringField hashVal
      "HashOctiles" -> ListField  octileContext <$> traverse (makeItem . hashOctile) [0 .. hashRows - 1]
      "HashOctile0" -> fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 0
      "HashOctile1" -> fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 1
      "HashOctile2" -> fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 2
      "HashOctile3" -> fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 3
      "HashOctile4" -> fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 4
      "HashOctile5" -> fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 5
      "HashOctile6" -> fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 6
      "HashOctile7" -> fmap (ListField defaultContext) . traverse (makeItem . pure) $ hashOctile 7
      "WordCount" -> str wc
      "LOC"       -> str loc
      "ERT"       -> let -- Note the estimated 265 words per minute
                         -- 226ms ~ 60s / 265
                         msWords = wc  * 226
                         -- We estimate 6.5s per line of code
                         msLOC   = loc * 6500
                         (m,r)   = (msWords + msLOC) `quotRem` 60000
                         reqMins = m + if r == 0 then 0 else 1
                     in  str reqMins
      _           -> noResult $ "Tried field " <> k

  where
    octileContext = Context $ \k _ i -> do
            StringField body <- unContext defaultContext "body" [] i
            case k of
              "HexChars" -> ListField defaultContext <$> traverse (makeItem . pure) body
              _          -> noResult $ "Tried field " <> k


-- |
-- An accumulator type to collect revevant blog post metrics in a single pass.
newtype PanDocAccum = A { unAccum :: (Word, Word, Text, ByteString) }


instance Semigroup PanDocAccum where

    (A (w,x,y,z)) <> (A (a,b,c,d)) = A (w+a,x+b,y<>c,z<>d)


instance Monoid PanDocAccum where

    mempty = A (0,0,mempty,mempty)


codeAccum, wordAccum :: Text -> PanDocAccum
codeAccum x =
    let ls = T.lines x
    in  A (0, toEnum $ length ls,         x, T.encodeUtf8 $ T.unlines ls)
wordAccum x =
    let ws = T.words x
    in  A (   toEnum $ length ws, 0, mempty, T.encodeUtf8 $ T.unwords ws)


-- |
-- Returns the:
--   - Word count
--   - Lines of code
--   - SHA3-512 hash
collateBlogPostMetadata :: Pandoc -> (Word, Word, Digest SHA3_512)
collateBlogPostMetadata doc@(Pandoc _ blocks) = (wordCount, codeCount, hashDigest)
  where
        (wordCount, codeCount, _, _) = unAccum $ cbs blocks

        renderTxtDoc = runPure . writePlain defaultHakyllWriterOptions
        tokenizeText = T.unwords . T.words . T.intercalate " " . T.lines
        hashDigest   = case renderTxtDoc doc of
                         Left  _   -> error "Could not serialize data for hashing"
                         Right txt -> hashWith SHA3_512 .T.encodeUtf8 $ tokenizeText txt

        cbs   ::   [Block]   -> PanDocAccum
        cbss  ::  [[Block]]  -> PanDocAccum
        cbs   = foldMap cb
        cbss  = foldMap cbs

        cb :: Block -> PanDocAccum
        cb (Plain         is )     = cis  is
        cb (Para          is )     = cis  is
        cb (LineBlock     iss)     = ciss iss
        cb (CodeBlock   _  s )     = codeAccum s
        cb (RawBlock    _  s )     = wordAccum s
        cb (BlockQuote    bs )     = cbs bs
        cb (OrderedList _ bss)     = cbss bss
        cb (BulletList    bss)     = cbss bss
        cb (DefinitionList ls)     = foldMap (\(is, bss) -> cis is <> cbss bss) ls
        cb (Header     _ _ is)     = cis is
        cb (Table _ _ _ th tbs tf) = fold [thi th, foldMap tbi tbs, tfi tf ]
        cb (Div _          bs)     = cbs bs
        cb HorizontalRule          = mempty
        cb Null                    = mempty

        cis  ::  [Inline]  -> PanDocAccum
        ciss :: [[Inline]] -> PanDocAccum
        cis  = foldMap ci
        ciss = foldMap cis

        ci :: Inline -> PanDocAccum
        ci (Str          s)        = wordAccum s
        ci (Emph        is)        = cis is
        ci (Underline   is)        = cis is
        ci (Span      _ is)        = cis is
        ci (Strong      is)        = cis is
        ci (Strikeout   is)        = cis is
        ci (Superscript is)        = cis is
        ci (Subscript   is)        = cis is
        ci (SmallCaps   is)        = cis is
        ci (Quoted    _ is)        = cis is
        ci (Cite      _ is)        = cis is
        ci (Code      _  s)        = wordAccum s
        ci (Math      _  s)        = wordAccum s
        ci (RawInline _  s)        = wordAccum s
        ci (Link      _ is (_, s)) = cis is <> wordAccum s
        ci (Image     _ is (_, s)) = cis is <> wordAccum s
        ci (Note        bs)        = cbs bs
        ci Space                   = singleWord
        ci SoftBreak               = singleWord
        ci LineBreak               = singleWord

        singleWord = A (1,0,mempty, mempty)

        thi (TableHead _ rs) = foldMap rowi rs
        tfi (TableFoot _ rs) = foldMap rowi rs
        tbi (TableBody _ _ rs1 rs2) = foldMap rowi (rs1 <> rs2)

        rowi  (Row _        cs) = foldMap celli cs
        celli (Cell _ _ _ _ bs) = cbs bs
