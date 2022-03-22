{-# LANGUAGE OverloadedStrings #-}

module PageBuilder.Blog
  ( buildBlogList
  , buildBlogPosts
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


buildBlogList :: Rules ()
buildBlogList =
    create ["blog.html"] . version "html" $ do
        route idRoute
        compile $ do
            blogPostList <- recentFirst =<< loadAll (blogPostPath .&&. hasVersion "html")
            let blogListContext = contextUsing
                  [  listField "BlogPostList" blogPostMetadataContext $ pure blogPostList
                  , constField "Title"  "Blog"
                  , constField "NavRef" "blog"
                  , constField "AddCSS" "blog"
                  ]

            makeItem ""
                >>= loadAndApplyTemplate blogListTemplate blogListContext
                >>= loadAndApplyTemplate templateDefault  blogListContext
                >>= finalizePage


buildBlogPosts :: Rules ()
buildBlogPosts =
    let blogPostUsing :: (Context String -> Pattern -> (String -> Routes) -> t) -> t
        blogPostUsing x = x blogPostContext blogPostPath pageRouteDefault
    in  do  compileFormatAsciiDoc `blogPostUsing` [ blogAsciiDocTemplate ]
            compileFormatEPUB     `blogPostUsing` []
            compileFormatPDF      `blogPostUsing` [ blogLatexTemplate ]
            compileFormatMarkdown `blogPostUsing` [ blogMarkdownTemplate ]
            compileFormatTextual  `blogPostUsing` []
            compileFormatHTML     `blogPostUsing` [ blogPostTemplate, templateDefault ]


blogPostMetadataContext :: Context String
blogPostMetadataContext = contextUsing
    [  dateField "Date"       "1%0Y+%j"
    ,  boolField "IsBlogList" $ const True
    , constField "NavRef"     "blog"
    ]


blogListTemplate :: Identifier
blogListTemplate = templateUsing "blog-list.html"


blogAsciiDocTemplate, blogMarkdownTemplate, blogPostTemplate, blogLatexTemplate :: Identifier
blogAsciiDocTemplate = templateUsing "blog-post.adoc"
blogMarkdownTemplate = templateUsing "blog-post.md"
blogPostTemplate     = templateUsing "blog-post.html"
blogLatexTemplate    = templateUsing "blog-post.latex"
