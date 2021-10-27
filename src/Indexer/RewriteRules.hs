{-# LANGUAGE OverloadedStrings #-}

module Indexer.RewriteRules
  ( constructRewriteRules
  ) where

import Hakyll


constructRewriteRules :: Rules ()
constructRewriteRules =
    create [".htaccess"] $ do
      route idRoute
      compile $ makeItem rewriteRules


rewriteRules :: String
rewriteRules = unlines
    [ "# To allow pages & directories to share names"
    , "DirectorySlash Off"
    , ""
    , "<IfModule mod_rewrite.c>"
    , "RewriteEngine On"
    , "# Begin EnforceSSL recursion.ninja"
    , "RewriteCond %{HTTP_HOST} ^(www.)?recursion.ninja$"
    , "RewriteCond %{HTTPS} !=on"
    , "RewriteRule ^(.*)$ https://%{HTTP_HOST}%{REQUEST_URI} [L]"
    , "# End EnforceSSL"
    , ""
    , "# Begin route trimming"
    , "RewriteCond %{THE_REQUEST} /([^.]+)\\.html [NC]"
    , "RewriteRule ^ /%1 [NC,L,R]"
    , "RewriteCond %{REQUEST_FILENAME}.html -f"
    , "RewriteRule ^ %{REQUEST_URI}.html [NC,L]"
--    , "RewriteCond %{REQUEST_FILENAME} !-f"
--    , "RewriteCond %{REQUEST_FILENAME} !-d"
--    , "RewriteRule ^(.*)\\.html$ /$1 [L,R=302]"
    , "# End route trimming"
    , "</IfModule>"
    ]
