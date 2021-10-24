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
import Data.Text              (Text, words)
import Hakyll
import Prelude                hiding (words)
import System.FilePath.Posix  ((</>))
import Text.Pandoc.Definition (Pandoc)

cvPageBuilder :: Rules ()
cvPageBuilder = do

    -- Full CV PDF
    compilePdfFormat
      cvContext
      cvPageInput
      staticPageRoute
      [ cvPdfTemplate ]

    -- Concise Résumé PDF
    compilePdfFormatWith
      cv2Resume
      "résumé"
      resumeContext
      cvPageInput
      resumeRoute
      [ cvPdfTemplate ]

    staticPageBuilder
      cvContext
      cvPageInput
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
cvPageInput = fromString $ pagePath </> "cv.md"


cvPageTemplate :: Identifier
cvPageTemplate = fromString $ templatePath </> "cv.html"


cvPdfTemplate :: Identifier
cvPdfTemplate = fromString $ templatePath </> "cv.latex"


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
resumeRoute = aliasPageRoute "resume.md"


cvConnectionImages :: Context String
cvConnectionImages = fold
    [ constField         "EmailImg" "img/email.png"
    , constField        "GitHubImg" "img/github.png"
    , constField   "LastUpdatedImg" "img/hourglass.png"
    , constField           "PDFImg" "img/pdf-logo.png"
    , constField "StackOverflowImg" "img/stackoverflow.png"
    , constField      "TimeZoneImg" "img/clock.png"
    , constField       "WebsiteImg" "img/website.png"
    ]
