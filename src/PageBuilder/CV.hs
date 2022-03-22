{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}

module PageBuilder.CV
    ( buildCV
    ) where

import Compiler.Constants
import Compiler.HTML
import Compiler.PDF
import Control.Applicative
import Data.Foldable
import Data.String (fromString)
import Data.Text (Text, drop, isPrefixOf, replace, words)
import Hakyll.Core.Identifier (Identifier)
import Hakyll.Core.Identifier.Pattern (Pattern)
import Hakyll.Core.Routes (Routes)
import Hakyll.Core.Rules (Rules)
import Hakyll.Web.Template.Context (Context, constField)
import Prelude hiding (drop, words)
import System.FilePath.Posix ((</>))
import Text.Pandoc.Definition (Block(..), Inline(..), Pandoc)
import Text.Pandoc.Walk


buildCV :: Rules ()
buildCV = do

    -- Full CV PDF
    compileFormatPDF cvContext cvPageInput pageRouteStatic [cvPdfTemplate]

    -- Concise Résumé PDF
    compileFormatPDFWith cv2Resume "résumé" resumeContext cvPageInput resumeRoute [cvPdfTemplate]

    compileFormatTransformedHTML
        transformForHTML
        cvContext
        cvPageInput
        pageRouteStatic
        [cvPageTemplate, templateDefault]


cvContext :: Context String
cvContext = contextUsing
    [ constField "AddCSS" "cv"
    , constField "NavRef" "cv"
    , constField "Title"  "Curriculum Vitae"
    , cvConnectionImages
    , flagField "HasDownloadFormats"
    , flagField "HasPDF"
    , flagField "IsCurriculumVitae"
    ]


cvPageInput :: Pattern
cvPageInput = fromString $ pathToPages </> "cv.md"


cvPageTemplate :: Identifier
cvPageTemplate = fromString $ pathToTemplates </> "cv.html"


cvPdfTemplate :: Identifier
cvPdfTemplate = fromString $ pathToTemplates </> "cv.latex"


cv2Resume :: Pandoc -> Pandoc
cv2Resume = blockFilter excludeSections
    where
        excludeSections :: [(Int, Text)]
        excludeSections =
            fmap (fold . words)
                <$> [ (1, "Publications & Presentations")
                    , (1, "Manuscripts in Preparation")
                    , (1, "Distinctions")
                    , (2, "Hunter College New York, NY")
                    , (2, "University of Wisconsin - Milwaukee Milwaukee, WI")
                    ]


resumeContext :: Context String
resumeContext = contextUsing [constField "Title" "Résumé", cvConnectionImages]


resumeRoute :: String -> Routes
resumeRoute = pageRouteAlias "resume.md"


cvConnectionImages :: Context String
cvConnectionImages = fold
    [ constField "EmailImg"         "email.png"
    , constField "GitHubImg"        "github.png"
    , constField "LastUpdatedImg"   "hourglass.png"
    , constField "PDFImg"           "pdf-logo.png"
    , constField "StackOverflowImg" "stackoverflow.png"
    , constField "TimeZoneImg"      "clock.png"
    , constField "WebsiteImg"       "website.png"
    ]


transformForHTML :: Pandoc -> Pandoc
transformForHTML = removeDataDirectory . niceDateSeperator


-- |
-- Replaces ASCII dashes with Unicode em dash in all code blocks which occur in
-- all headers.
niceDateSeperator :: Pandoc -> Pandoc
niceDateSeperator =
    let f (Header n a is) = Header n a $ walk h is
        f x               = x
        h (Code a txt) = Code a $ replace " - " " — " txt
        h x            = x
    in  walk f


-- |
-- Removes the prefix @data/@ from any image reference URL.
-- We do this because we cannot have the HTML page reference @img/doi.png@ while
-- also having the LaTeX documents reference @data/img/doi.png@, but both references
-- are required for a successfully compiled website.
-- This conundrum is solved by specifying @data/img/doi.png@ in the original
-- markdown file and prunning off the @data/@ prefix during HTML generation and
-- preserving the @data/@ prefix during LaTeX generation.
--
-- /It __is__ a bit ugly, but it works!/
removeDataDirectory :: Pandoc -> Pandoc
removeDataDirectory =
    let f = \case
            Image a b (url, c) | "data/" `isPrefixOf` url -> Image a b (drop 5 url, c)
            x                                             -> x
    in  walk f
