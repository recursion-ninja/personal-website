{-# Language OverloadedStrings #-}

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
import Data.Foldable (traverse_)
import Hakyll


buildBlogList :: Rules ()
buildBlogList = create ["blog.html"] . version "html" $ do
    route idRoute
    compile $ do
        blogPostList <- recentFirst =<< loadAll (blogPostPath .&&. hasVersion "html")
        let blogListContext = contextUsing
                [ listField "BlogPostList" blogPostMetadataContext $ pure blogPostList
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
    let blogPostTypeUsing :: (Context String -> Pattern -> (String -> Routes) -> b -> t, b) -> t
        blogPostTypeUsing (x, y) = x blogPostContext blogPostPath pageRouteDefault y
    in  traverse_
        blogPostTypeUsing
        [ (compileFormatAsciiDoc, [blogAsciiDocTemplate])
        , (compileFormatEPUB    , [])
        , (compileFormatPDF     , [blogLatexTemplate])
        , (compileFormatMarkdown, [blogMarkdownTemplate])
        , (compileFormatTextual , [])
        , (compileFormatHTML    , [blogPostTemplate, templateDefault])
        ]


blogPostMetadataContext :: Context String
blogPostMetadataContext =
    contextUsing [dateField "Date" "1%0Y+%j", flagField "IsBlogList", constField "NavRef" "blog"]


blogListTemplate :: Identifier
blogListTemplate = templateUsing "blog-list.html"


blogAsciiDocTemplate, blogMarkdownTemplate, blogPostTemplate, blogLatexTemplate :: Identifier
blogAsciiDocTemplate = templateUsing "blog-post.adoc"
blogMarkdownTemplate = templateUsing "blog-post.md"
blogPostTemplate = templateUsing "blog-post.html"
blogLatexTemplate = templateUsing "blog-post.latex"
