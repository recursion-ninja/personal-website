{-# LANGUAGE OverloadedStrings #-}

module Compiler.Transformation.HeaderExclusion (
    -- * Exclude Specified Headers
    excludeHeadersBy,
) where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.STRef
import Data.Text (Text, words)
import Text.Pandoc.Definition
import Prelude hiding (words)


{- |
Exclude the specified headers from the resulting 'Pandoc' document.

Headers will be excluded when the following criteria are met:
  * The current header has the same header level as the previous header
  * The current header's textual content matches an element in the provided set.
-}
excludeHeadersBy ∷ (Foldable f, Functor f) ⇒ f (Int, Text) → Pandoc → Pandoc
excludeHeadersBy excludedHeaders (Pandoc meta blocks) = Pandoc meta $ fold blocks'
    where
        blocks' ∷ [[Block]]
        blocks' = runST $ do
            exclusionSet ← newSTRef excludedHeaders
            excluded ← newSTRef Nothing
            let setExcluded = writeSTRef excluded
            let filterExcludes input =
                    let title ∷ Text
                        (_, title) = getHeaderTitle input

                        decrementMatches ∷ (Functor f) ⇒ f (Int, Text) → f (Int, Text)
                        decrementMatches = fmap decrementOnMatch

                        decrementOnMatch ∷ (Int, Text) → (Int, Text)
                        decrementOnMatch e@(n, ht)
                            | ht == title = (n - 1, ht)
                            | otherwise = e
                    in  do
                            es ← readSTRef exclusionSet
                            ex ← readSTRef excluded
                            case ex of
                                Nothing → pure ()
                                Just val → when (isSameBlockType val input) $ setExcluded Nothing
                            when ((1, title) `elem` es) $ setExcluded (Just input)
                            modifySTRef exclusionSet decrementMatches
                            ex' ← readSTRef excluded
                            pure $ maybe [input] (const []) ex'
            traverse filterExcludes blocks


clearSpaces ∷ Text → Text
clearSpaces = fold . words


getHeaderTitle ∷ Block → (Int, Text)
getHeaderTitle (Header n _ lineParts) = (n, foldMap inlineText lineParts)
getHeaderTitle _ = (0, mempty)


inlineText ∷ Inline → Text
inlineText (Str txt) = clearSpaces txt
inlineText (Emph ts) = foldMap inlineText ts
inlineText (Underline ts) = foldMap inlineText ts
inlineText (Strong ts) = foldMap inlineText ts
inlineText (Strikeout ts) = foldMap inlineText ts
inlineText (Superscript ts) = foldMap inlineText ts
inlineText (Subscript ts) = foldMap inlineText ts
inlineText (SmallCaps ts) = foldMap inlineText ts
inlineText (Quoted _ ts) = foldMap inlineText ts
inlineText (Cite _ ts) = foldMap inlineText ts
inlineText (Code _ txt) = clearSpaces txt
inlineText (Math _ txt) = clearSpaces txt
inlineText (RawInline _ txt) = clearSpaces txt
inlineText (Link _ ts _) = foldMap inlineText ts
inlineText (Image _ ts _) = foldMap inlineText ts
inlineText (Span _ ts) = foldMap inlineText ts
inlineText _ = ""


isSameBlockType ∷ Block → Block → Bool
isSameBlockType Plain{} Plain{} = True
isSameBlockType Para{} Para{} = True
isSameBlockType LineBlock{} LineBlock{} = True
isSameBlockType CodeBlock{} CodeBlock{} = True
isSameBlockType RawBlock{} RawBlock{} = True
isSameBlockType BlockQuote{} BlockQuote{} = True
isSameBlockType OrderedList{} OrderedList{} = True
isSameBlockType BulletList{} BulletList{} = True
isSameBlockType DefinitionList{} DefinitionList{} = True
isSameBlockType (Header x _ _) (Header y _ _) = x == y
isSameBlockType HorizontalRule HorizontalRule = True
isSameBlockType Table{} Table{} = True
isSameBlockType Div{} Div{} = True
isSameBlockType _ _ = False
