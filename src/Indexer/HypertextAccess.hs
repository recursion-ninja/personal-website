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
    , "# Begin custom error code pages"
    , "ErrorDocument 400 /400.html"
    , "ErrorDocument 404 /404.html"
    , "ErrorDocument 500 /500.html"
    , ""
    , "# Enforce SSL/TLS"
    , "RewriteCond %{HTTP_HOST} ^(www\\.)?(.+)$"
    , "RewriteCond %{HTTPS} !=on"
    , "RewriteRule ^(.*)$ https://%2%{REQUEST_URI} [L,R]"
    , ""
    , "# Removing www subdomain from URL"
    , "RewriteCond %{HTTP_HOST} ^www\\.(.+)$ [NC]"
    , "RewriteRule ^(.*)$ https://%1%{REQUEST_URI} [L,QSA,NC,R]"
    , ""
    , "# Re-route blog directory to blog listing"
    , "RewriteCond %{REQUEST_URI} ^(/blog)/$ [NC]"
    , "RewriteRule ^ %1 [L,R]"
    , ""
    , "# Remove HTML extension from route suffix"
    , "RewriteCond %{THE_REQUEST} /([^.]+)\\.html [NC]"
    , "RewriteRule ^ /%1 [NC,L,R]"
    , "RewriteCond %{REQUEST_FILENAME}.html -f"
    , "RewriteRule ^ %{REQUEST_URI}.html [NC,L]"
    , "</IfModule>"
    ]
