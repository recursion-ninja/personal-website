{-# LANGUAGE OverloadedStrings #-}

module PageBuilder.Blog
  ( blogListBuilder
  , blogPostBuilder
  , blogPostContext
  , blogPostPath
  ) where

import Compiler.AsciiDoc
import Compiler.BlogPostContext
import Compiler.Constants
import Compiler.EPUB
import Compiler.HTML
import Compiler.Markdown
import Compiler.PDF
import Compiler.Textual
import Hakyll


blogListBuilder :: Rules ()
blogListBuilder =
    create ["blog.html"] . version "html" $ do
        route idRoute
        compile $ do
            blogPostList <- recentFirst =<< loadAll (blogPostPath .&&. hasVersion "html")
            let blogListContext = makeContext
                  [  listField "BlogPostList" blogPostMetadataContext $ pure blogPostList
                  , constField "Title"  "Blog"
                  , constField "NavRef" "blog"
                  , constField "AddCSS" "blog"
                  ]

            makeItem ""
                >>= loadAndApplyTemplate blogListTemplate blogListContext
                >>= loadAndApplyTemplate defaultTemplate  blogListContext
                >>= pageFinalizer


blogPostBuilder :: Rules ()
blogPostBuilder = do

    compileAsciiDocFormat
      blogPostContext
      blogPostPath
      pageRouteDefault
      [ blogAsciiDocTemplate ]

    compileEpubFormat
      blogPostContext
      blogPostPath
      pageRouteDefault

    compilePdfFormat
      blogPostContext
      blogPostPath
      pageRouteDefault
      [ blogLatexTemplate ]

    compileMarkdownFormat
      blogPostContext
      blogPostPath
      pageRouteDefault
      [ blogMarkdownTemplate ]

    compileTextualFormat
      blogPostContext
      blogPostPath
      pageRouteDefault
      []

    pageBuilder
      blogPostContext
      blogPostPath
      pageRouteDefault
      [ blogPostTemplate
      , defaultTemplate
      ]


blogPostMetadataContext :: Context String
blogPostMetadataContext = makeContext
    [  dateField "Date"       "1%0Y+%j"
    ,  boolField "IsBlogList" $ const True
    , constField "NavRef"     "blog"
    ]


blogListTemplate :: Identifier
blogListTemplate = makeTemplate "blog-list.html"


blogAsciiDocTemplate, blogMarkdownTemplate, blogPostTemplate, blogLatexTemplate :: Identifier
blogAsciiDocTemplate = makeTemplate "blog-post.adoc"
blogMarkdownTemplate = makeTemplate "blog-post.md"
blogPostTemplate     = makeTemplate "blog-post.html"
blogLatexTemplate    = makeTemplate "blog-post.latex"
