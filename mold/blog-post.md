```$for(HashOctiles)$
$for(HexChars)$$body$$endfor$$endfor$
```

| | |
|:-----------|:-|
$if(Title)$| Title      | $Title$ |
$endif$$if(Subtitle)$| Subtitle   | $Subtitle$ |
$endif$$if(Author)$| Author     | $Author$ |
$endif$$if(Date)$| Date       | $Date$ |
$endif$$if(WordCount)$| Word Count | $WordCount$ (ERT $ERT$ min) |
$endif$$if(LOC)$| Code Lines | $LOC$ |
$endif$| Formats |$if(HasASCIIDoc)$ [.adoc]($BaseURL$$BaseRoute$.adoc)$endif$$if(HasEPUB)$[.epub]($BaseURL$$BaseRoute$.epub)$endif$ [.html]($BaseURL$$BaseRoute$.html) $if(HasMarkdown)$[.markdown]($BaseURL$$BaseRoute$.md)$endif$ $if(HasPDF)$[.pdf]($BaseURL$$BaseRoute$.pdf)$endif$$if(HasTextual)$[.txt]($BaseURL$$BaseRoute$.txt)$endif$ |

$body$
