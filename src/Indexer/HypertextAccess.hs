{-# Language OverloadedStrings #-}

module Indexer.HypertextAccess
    ( constructHypertextAccess
    ) where

import Hakyll.Core.Compiler (makeItem)
import Hakyll.Core.Routes (idRoute)
import Hakyll.Core.Rules (Rules, compile, create, route)


constructHypertextAccess :: Rules ()
constructHypertextAccess = create [".htaccess"] $ do
    route idRoute
    compile $ makeItem rewriteRules


rewriteRules :: String
rewriteRules = unlines
    [ "# To allow pages & directories to share names"
    , "DirectorySlash Off"
    , ""
    , "<IfModule mod_rewrite.c>"
    , "RewriteEngine On"
    , ""
    , "# Begin EnforceSSL recursion.ninja"
    , "RewriteCond %{HTTP_HOST} ^(www.)?recursion.ninja$"
    , "RewriteCond %{HTTPS} !=on"
    , "RewriteRule ^(.*)$ https://%{HTTP_HOST}%{REQUEST_URI} [L]"
    , ""
    , "# Begin route trimming"
    , "RewriteCond %{THE_REQUEST} /([^.]+)\\.html [NC]"
    , "RewriteRule ^ /%1 [NC,L,R]"
    , "RewriteCond %{REQUEST_FILENAME}.html -f"
    , "RewriteRule ^ %{REQUEST_URI}.html [NC,L]"
    , ""
    , "# Begin custom error code pages"
    , "ErrorDocument 400 /400.html"
    , "ErrorDocument 404 /404.html"
    , "ErrorDocument 500 /500.html"
    , "</IfModule>"
    ]
