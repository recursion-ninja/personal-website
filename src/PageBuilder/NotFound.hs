module PageBuilder.NotFound
  ( notFoundPageBuilder
  ) where

import Compiler.Constants
import Compiler.HTML
import Data.String
import Hakyll
import System.FilePath.Posix ((</>))


notFoundPageBuilder :: Rules ()
notFoundPageBuilder =
    staticPageBuilder
      notFoundContext
      notFoundPageInput
      [defaultTemplate]


notFoundContext :: Context String
notFoundContext = makeContext
    [ constField "Title" "404"
    ]


notFoundPageInput :: Pattern
notFoundPageInput = fromString $ pagePath </> "404.html"
