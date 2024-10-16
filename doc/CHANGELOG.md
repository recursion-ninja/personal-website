# Revision history

The changelog utilized [Semantic Versioning (v2.0.0)][1] and is available [on GitHub][GitHub-Changelog].

[GitHub-Changelog]: https://github.com/recursion-ninja/personal-website/blob/master/doc/CHANGELOG.md

## Unreleased

  * *None*


## 2.3.1 -- 12024-10-11

  * Updated to support GHC-9.10.1


## 2.3.0 -- 12024-04-19

  * Refactored CV & Resume generation


## 2.2.20 -- 12024-03-31

  * Corrected code syntax highlighting in PDF outputs

  * Updated CV & Resume content


## 2.2.19 -- 12024-01-20

  * Corrected many spelling errors

  * Transition from deprecated GitHub workflow 'haskell/action/setup' to 'haskell-actions/setup'

  * Updated CV & Resume content

  * Updated GitHub workflow configuration

  * Updated spelling checker `cspell` configuration and extra dictionaries

  * Updated to support GHC-9.8.1

  * Updated to support GHC-9.4.8

  * Updated to support GHC-9.2.8


## 2.2.18 -- 12023-11-03

  * Updated CV & Resume content


## 2.2.17 -- 12023-09-06

  * Added `fourmolu` styling to code-base

  * Updated About webpage

  * Updated CV & Resume content


## 2.2.16 -- 12023-06-29

  * Added table styling for weblog posts

  * Corrected broken table LaTeX generation

  * Corrected code block rendering in HTML

  * Corrected figure rendering in HTML

  * Enhanced Pandoc reader with `grid_tables` extension

  * Enhanced PDF rendering to optionally use the `xelatex` engine instead of `pdflatex`

  * Enhanced rendering of tables in HTML output

  * Enhanced rendering of tables in PDF output

  * Updated CV & Resume context

  * Updated to support GHC-9.4.5

  * Updated to support GHC-9.2.7

  * Updated by dropping support for GHC-8.10.7


## 2.2.15 -- 12023-05-07

  * Corrected CSS rending of DOI links on CV page

  * Increased `line-height` value for easier reading

  * Updating CV content


## 2.2.14 -- 12023-03-29

  * Posted weblog: "Responsible A.I. Admonition"


## 2.2.13 -- 12023-03-17

  * Added responsive rendering of `figure` elements, rising to fit small viewports

  * Corrected many spelling errors

  * Posted weblog: "Conspectus 12023 Q1"


## 2.2.12 -- 12023-03-15

  * Corrected many spelling errors

  * Posted weblog: "Parsing Permutations"

  * Updated to support GHC-9.4.4


## 2.2.11 -- 12023-01-20

  * Added asterism section separators in HTML weblog posts

  * Added editorial coronis terminator in HTML weblog posts

  * Corrected CSS rendering inconsistencies

  * Improved rendering of "data lists"

  * Posted weblog: "Conspectus 12022 Q4"

  * Updated to support GHC-9.2.5

  * Updated WSC-2021 link to use proper DOI reference


## 2.2.10 -- 12022-10-19

  * Added [SCC](https://github.com/boyter/scc) to CI

  * Added [StAn](https://github.com/kowainik/stan) to CI

  * Added [Weeder](https://github.com/ocharles/weeder) to CI

  * Corrected favicon references

  * Corrected license information in `meta` tag

  * Posted weblog: "Conspectus 12022 Q3"

  * Updated "About" page with new professional synopsis

  * Updated `manifest.json` with more available favicons

  * Updated `meta` and `link` tag content for readability


## 2.2.9 -- 12022-08-28

  * Added [MegaLinter](https://megalinter.github.io) to CI

  * Updating contents to conform to MegaLinter suggestions


## 2.2.8 -- 12022-07-05

  * Added continuous integration (CI) via GitHub Actions

  * Corrected many spelling errors

  * Posted weblog: "Conspectus 12022 Q2"

  * Updated hypertext access file


## 2.2.7 -- 12022-05-31

  * Corrected rendering of inline code blocks

  * Updated software `LICENSE`


## 2.2.6 -- 12022-05-30

  * Adding copyright to website footer


## 2.2.5 -- 12022-03-25

  * Corrected posting date of weblog: "Conspectus 12022 Q1"


## 2.2.4 -- 12022-03-25

  * Posted weblog: "Conspectus 12022 Q1"


## 2.2.3 -- 12022-03-22

  * Corrected DOI image and table rendering issues

  * Updated CV


## 2.2.2 -- 12022-03-22

  * Added explicit import lists for `Hakyll` modules

  * Added dynamic specification of website domain, no longer hardcoded everywhere

  * Refactored exported function names to be more consistent and intuitive

  * Refactored codebase layout:

    - Added `AssetIncludes` module for copying/compiling "asset" files

    - Added `config` directory for configurable build tool files

    - Moved `css`, `fav`, `img`, `mold`, and `page` into `data` directory

    - Renamed `mold` to `template` directory

  * Updated `.cabal` file package description

  * Updated to support GHC-9.2.2


## 2.2.1 -- 12022-03-18

  * Refactored code based on new compiler warnings

  * Moved `CHANGELOG`, `LICENSE`, and `README` to new `doc` directory


## 2.2.0 -- 12022-03-17

  * Posted weblog: "Matriculation"


## 2.1.10 -- 12022-03-17

  * Updated build parameters of `cabal.project`

  * Updated required cabal version of the of `.cabal` file


## 2.1.9 -- 12021-12-06

  * Minor information updates to CV


## 2.1.8 -- 12021-12-06

  * Added direct link to Winter Simulation Conference 2021 paper


## 2.1.7 -- 12021-11-29

  * Updated About page, again


## 2.1.6 -- 12021-11-29

  * Updated About page


## 2.1.5 -- 12021-11-21

  * Added manuscript to CV

  * Corrected DOI link in CV

  * Updated to support GHC-9.0.1


## 2.1.4 -- 12021-10-29

  * Changed subtitle of a weblog post

  * Ensure only `.md` files in `blog` are processed as weblog posts


## 2.1.3 -- 12021-10-28

  * Added a Pandoc transformation before HTML rendering

  * Extended `.htaccess` routing for some custom error pages


## 2.1.2 -- 12021-10-27

  * Added `.htaccess` file generation for:

    - Redirect to enforce SSL

    - Route trimming of HTML pages


## 2.1.1 -- 12021-10-27

  * Fixed rendering defect in footer on small screens


## 2.1.0 -- 12021-10-27

  * Revising and reposting first weblog entry


## 2.0.4 -- 12021-10-26

  * Correcting GPA on CV


## 2.0.3 -- 12021-10-25

  * Beautified font-size on small screens

  * Abbreviated dates and standardizing their spacing on CV


## 2.0.1 -- 12021-10-24

  * Corrected a typo on the `About` page


## 2.0.0 -- 12021-10-24

  * Second major incarnation; initially released on an unsuspecting world


