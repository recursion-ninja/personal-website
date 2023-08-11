{-# LANGUAGE LambdaCase #-}

module Compiler.Transformation.Table (
    -- * Transformer for Table blocks
    equalizeTableColumns,
) where

import Text.Pandoc.Definition (Block (Table), ColSpec, ColWidth (..), Pandoc)
import Text.Pandoc.Walk (walk)


{- |
Transform each 'Table' within the 'Pandoc' such that for each table, all columns
of that that will have equal width.

This alters the behavior of Pandoc which (typically) uses whitespace to interpret
how wide the columns of a table should be relative to each other.
-}
equalizeTableColumns ∷ Pandoc → Pandoc
equalizeTableColumns =
    let alterTable ∷ Block → Block
        alterTable = \case
            Table attr caption colsPec tHead tBody tFoot →
                let uniformColSpec = equalizeColumnSpecifications colsPec
                in  Table attr caption uniformColSpec tHead tBody tFoot
            x → x
    in  walk alterTable


{- |
Ensures that all columns have equal spacing.
-}
equalizeColumnSpecifications ∷ [ColSpec] → [ColSpec]
equalizeColumnSpecifications colSpecs =
    let equal ∷ ColWidth
        equal = ColWidthDefault
        --        equal = ColWidth $ 1 / toEnum (length colSpecs)

        equalize ∷ ColSpec → ColSpec
        equalize = fmap (const equal)
    in  equalize <$> colSpecs
