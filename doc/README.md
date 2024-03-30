## Personal Website of Recursion Ninja


The live site can be viewed here:

https://recursion.ninja


### Installation

```bash
$ cabal build personal-website
$ cabal exec  personal-website clean
$ cabal exec  personal-website build
$ cabal exec  personal-website server
```

*One-liner:*
```bash
for a in clean build server; do cabal run . -- $a; done
```

The compiled website will be placed in the `_site` directory.


### Running the linters

```bash
$ mega-linter-runner
```