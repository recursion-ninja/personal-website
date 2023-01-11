{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}

module PageBuilder.Blog
    ( buildBlogList
    , buildBlogPosts
    ) where

import Compiler.AsciiDoc
import Compiler.BlogPostContext
import Compiler.Constants
import Compiler.EPUB
import Compiler.Generic (FormatCompiler)
import Compiler.HTML
import Compiler.Markdown
import Compiler.PDF
import Compiler.Textual
import Data.Foldable (traverse_)
import Data.Text (drop, isPrefixOf)
import Hakyll.Core.Compiler (loadAll, makeItem)
import Hakyll.Core.Identifier (Identifier)
import Hakyll.Core.Identifier.Pattern (Pattern, hasVersion, (.&&.))
import Hakyll.Core.Routes (Routes, idRoute)
import Hakyll.Core.Rules (Rules, compile, create, route, version)
import Hakyll.Web.Template (loadAndApplyTemplate)
import Hakyll.Web.Template.Context (Context, constField, dateField, listField)
import Hakyll.Web.Template.List (recentFirst)
import Prelude hiding (drop)
import Text.Pandoc.Definition (Inline(..), Pandoc)
import Text.Pandoc.Walk (walk)


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
        , (compileFormatBlogHTML, [blogPostTemplate, templateDefault])
        ]



compileFormatBlogHTML :: FormatCompiler
compileFormatBlogHTML =
    let alterDataDirectory :: Pandoc -> Pandoc
        alterDataDirectory = walk transform


        transform = \case
            Image a b (url, c) | "data/" `isPrefixOf` url -> Image a b (".." <> drop 4 url, c)
            x                                             -> x

    in  compileFormatTransformedHTML alterDataDirectory


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
