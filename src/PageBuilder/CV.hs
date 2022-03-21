{-# LANGUAGE OverloadedStrings #-}

module PageBuilder.CV
  ( cvPageBuilder
  ) where

import Compiler.Constants
import Compiler.HTML
import Compiler.PDF
import Control.Applicative
import Data.Foldable
import Data.String            (fromString)
import Data.Text              (Text, replace, words)
import Hakyll
import Prelude                hiding (words)
import System.FilePath.Posix  ((</>))
import Text.Pandoc.Definition (Pandoc, Block(..), Inline(..))
import Text.Pandoc.Walk

cvPageBuilder :: Rules ()
cvPageBuilder = do

    -- Full CV PDF
    compilePdfFormat
      cvContext
      cvPageInput
      pageRouteStatic
      [ cvPdfTemplate ]

    -- Concise Résumé PDF
    compilePdfFormatWith
      cv2Resume
      "résumé"
      resumeContext
      cvPageInput
      resumeRoute
      [ cvPdfTemplate ]

    pageBuilderWith
      transformForHTML
      cvContext
      cvPageInput
      pageRouteStatic
      [ cvPageTemplate
      , defaultTemplate
      ]


cvContext :: Context String
cvContext = makeContext
    [  boolField "HasDownloadFormats" $ const True
    ,  boolField "HasPDF"             $ const True
    ,  boolField "IsCurriculumVitae"  $ const True
    , constField "AddCSS" "cv"
    , constField "NavRef" "cv"
    , constField "Title"  "Curriculum Vitae"
    , cvConnectionImages
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
    excludeSections = fmap (fold . words) <$>
        [ (1, "Publications & Presentations")
        , (1, "Manuscripts in Preparation")
        , (1, "Distinctions")
        , (2, "Hunter College New York, NY")
        , (2, "University of Wisconsin - Milwaukee Milwaukee, WI")
        ]


resumeContext :: Context String
resumeContext = makeContext
    [ constField "Title" "Résumé"
    , cvConnectionImages
    ]

resumeRoute :: String -> Routes
resumeRoute = pageRouteAlias "resume.md"


cvConnectionImages :: Context String
cvConnectionImages = fold
    [ constField         "EmailImg" $ pathToImages </> "email.png"
    , constField        "GitHubImg" $ pathToImages </> "github.png"
    , constField   "LastUpdatedImg" $ pathToImages </> "hourglass.png"
    , constField           "PDFImg" $ pathToImages </> "pdf-logo.png"
    , constField "StackOverflowImg" $ pathToImages </> "stackoverflow.png"
    , constField      "TimeZoneImg" $ pathToImages </> "clock.png"
    , constField       "WebsiteImg" $ pathToImages </> "website.png"
    ]


transformForHTML :: Pandoc -> Pandoc
transformForHTML = niceDateSeperator


-- |
-- Replaces ASCII dashes with Unicode em dash in all code blocks which occur in
-- all headers.
niceDateSeperator :: Pandoc -> Pandoc
niceDateSeperator = walk f
  where
    f (Header n a is) = Header n a $ walk h is
    f x = x
    
    h (Code a txt) = Code a $ replace " - " " — " txt
    h x = x
