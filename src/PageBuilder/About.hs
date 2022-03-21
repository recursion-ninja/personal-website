module PageBuilder.About
  ( aboutPageBuilder
  ) where

import Compiler.Constants
import Compiler.HTML
import Data.String
import Hakyll
import System.FilePath.Posix ((</>))


aboutPageBuilder :: Rules ()
aboutPageBuilder =
    staticPageBuilder
      aboutContext
      aboutPageInput
      [defaultTemplate]


aboutContext :: Context String
aboutContext = makeContext
    [ constField "NavRef" "about"
    , constField "Title"  "About"
    ]


aboutPageInput :: Pattern
aboutPageInput = fromString $ pathToPages </> "about.md"
