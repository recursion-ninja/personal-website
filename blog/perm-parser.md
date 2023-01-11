---
Title:    Parsing Permutations
Subtitle: Trees, Temporality, and Termination
Author:   Recursion Ninja
date:     2022-05-30
---


# Preamble

Since 2015 I have intermittently worked with [Mark Karpov][gh-mrkkrp] on the venerable [`megaparsec`][mp-package] Haskell package.
With the release of [`megaparsec-6.0.0`][mp-v6.0.0], all polymorphic parsing combinators were moved to the newly released [`parser-combinators`][pc-package] package.
Unlike `megaparsec`, `parser-combinators` is a fully generic parsing package with a truly minimal dependancy footprint! 
According to a hackage reverse dependency query, `parser-combinators` is a *direct* [depenacy of 63 packages][pc-rev-dep] at the time of writing, so ecosystem adoption is modest but notable.

With the release of [`parser-combinators-0.2.0`][pc-v0.2.0] I added the [`Control.Applicative.Permutations`][pc-module] module, and since then my maintenance contributions to both `megaparsec` and `parser-combinators` has waned to primarily this sole module.
Despite the module's small interface, simple type signatures, and succinct code, I greatly appreciate the practicality provided as well as the theoretical underpinnings.
The algorithm I used for efficiently parsing arbitrary permutations of combinators was neither original nor recent work.
Published in 2004, [*"Parsing Permutation Phrases"*][DOI-0] describes astonishingly elegant funtional definitions for representing and processing permutations.
The work is deserving of the attributed allacode ["Functional Pearl."][fun-pearl]
A reviewer comparing the Haskell code within the module to the decades old paper would find immediately recognizable similarities as well as a few compatability modifications required to bring the original work upto the modern Haskell `Prelude`.
While the correctness and elegance of this pearl will never age, what follows is a meager insight relating permutation parsing and [Computation Tree Logic][wiki-CTL].


# Postulate

My present comprehension of and commentary on permutation paring did not manifest from the ether.
The inciting incedent emerged as an evidently [innocent issue][gh-feature].
A feature request was made.


Permutation Parsers
====================

## Persistent problems in perpetual processing


There are going to be some technical problems with parsing permutations with the semantics you are requesting.

Let's consider the following while unpack how the permutation parser (currently) works:

**Permutation Parser:**

```
example = (,,) 
    <$> char 'a' 
    <*> manyPerm (char 'b') 
    <*> somePerm (chat 'c')
```

**Input String:**

```
acceptable = "ccba"
nonhalting = "ccbacbcb"
```

**Desired Outputs:**
```
success = ('a', "b", "cc")
failure = ('a', "bbb", "ccc")

```


## Permutation Parsing Mechanics

The implementation of permutations paring is based on the functional pearl paper (citation impending). 


### Alternation Trees

The way permutations are parsed is via a tree structure. The height of the parsing tree is exactly `N` where `N` is the number of parser combinators used to construct the permutation parser. For the permutation parser `example` defined above `N=3`. The number of children at a node is equal to the node's height. For the permutation parser `example` defined above, the root node at the "top" of the tree has `3` children, each of these children on second layer has `2` children, the third layer has `1` child, and finally the leaves have zero children. Nodes in the tree represent permutation parser state and edges correspond to a successful parse result of one combinator.


### Nondeterministic computation 

The parser evaluation is best thought of as a nondeterministic sequence of partial computations which occur while traversing the tree in a breadth first, layer by layer traversal, terminating when it arrives at a single leaf node representing a complete permutation. At the root of the tree the permutation parser attempts to parse each of the `N` possible combinator. The edges leading from the root node to the second layer of nodes correspond to successfully parsing one of the combinators. For each successfully parsed combinator at the root node, the computation follows the corresponding edge to the node in the second layer. Note that not all nodes on the second layer are visited. A parse failure of a combinator means that the corresponding edge to the second layer will not be traversed and the whole subtree will be pruned from the computation. 

Each node in the second layer uniquely represents the partial computation state of `1` successful combinator parse result. The partial computation at the node contains the result of the permutation combinator which lead into the node as well the partially consumed input stream missing the elements consumed in parsing the associated combinator result. The nondeterministic computation continues on the second level with each visited node independently resuming the partial computation. The nodes attempt to parse the `N-1` combinators remaining, excluding the `1` combinator which has a parse result stored in the node's partial computation. Similarly to the root node, for each successfully parsed combinator at the node in the second layer, the computation follows the corresponding edge to the node in the third layer. Again, a parse failure of a combinator on the second layer means that the corresponding edge to the third layer will not be traversed and the whole subtree will be pruned from the computation.

This form of nondeterministically extending partial computations down a tree, pruning the search space on combinator parse failures continues. Generally speaking, when parsing a permutation with `N` combinators, a tree of height `N` is produced. The root node is said to have height `N` and the leaves are said to have height `0`. On the layer with height `x`, each node contains a partial computation with `N-x` combinator results from the path to the root node and there are `x` remaining combinators which must be parsed in the node's (inclusive) subtree. The nodes within the layer with height `x` each attempt to parse thier unique set of `x` remaining combinators, excluding the unique set of `N-x` combinators which parse results are already stored in the node's partial computation. For each successfully parsed combinator, the computation follows the corresponding edge to the node in the layer with height `x-1`. A parse failure of a combinator corresponding edge to the layer with height `x-1` will not be traversed and the subtree beneath the untraversed edge will be pruned from the computation.

When a leaf node is reached, the height is `x=0` and there are `N-x=N`unique successfully parsed combinators and `x=0` remaining combinators. Hence reaching a leaf node in the nondeterministic computation terminates the computation with a successful permutation parse result, each combinator being parsed once. The first leave node reached ends the entire nondeterministic computation with a successful permutation parse result. If no leaf node is reached, the permutation parser fails.



### The computational tree of `example`

With the permutation parser evaluation described above, there are exactly `6` regular expressions which will be accepted by the permutation parser `example`.

  1. `ab*c+`
  2. `ac+b*`
  3. `b*ac+`
  4. `b*c+a`
  5. `c+ab*`
  6. `c+b*a`

Below is a rendering of the generic permutation parse tree  The symbols `(...)` represent edge transition which correspond to the enclosed combinator. The `[...]` symbols spanning two lines represent a node's parser state.

![parse tree](data/img/perm-parser-00.png)

```
                [___]
            /     |   \
          /       |     \
        /         |       \
      /           |         \
   ( a )        ( b*)      ( c+)
     |            |          |
   [a__]        [_b_]      [__c]
   /   \        /   \      /   \
( b*) ( c+) ( a ) ( c+) ( a ) ( b*)
  |     |     |     |     |     |
[ab_] [a_c] [a_b] [_bc] [a_c] [_bc]
  |     |     |     |     |     |
( c+) ( b*) ( c+) ( a ) ( b*) ( a )
  |     |     |     |     |     |
[abc] [abc] [abc] [abc] [abc] [abc]

ab*c+ ac+b* b*ac+ b*c+a c+ab* c+b*a
```

Note how each path from the root to a leaf corresponds to a ***unique*** permutation of `example`'s component combinators! Furthermore, the nodes along the path from each root to each leaf produce a unique sequence of partial computations which, when composed sequentially, produce a unique parsing computation for each permutation.
The computation tree, by construction, always terminates.


### Performance considerations

With this nondeterministic tree evaluation algorithm, we are certain to successfully parse a constructed "Permutation Parser" if and only if the prefix of the input stream matches a permutation the component combinators. Elegantly, the tree structure of the algorithm memoizes partial computations, preventing a full look ahead an full backtracking on permutation parser evaluations which initially succeed on one or more component combinators then subsequently fail on a later combinator. Furthermore, lazy evaluation means that the nonterministic algorithm does not explore the whole tree. Instead it performs a depth first search with the described early abort/short-circuiting of a subtree if the associated combinator fails to parse. Additionally, because in practice the combinator attempts are evaluated single threaded in the order of composition, a clever author can order the component combinators in order of success probability. The practical result of this algorithm coupled with lazy, single-threaded, order-depenent evaluation is the following: 

> If all component parsers are pair-wise mutually exclusive, the algorithm will only proceed down one path of the computational tree, with `0` redundant parse attempts of component combinators in the best case and `N(N-1)/2` redundant parse attempts of component combinators (wasted work) in the worst case. While `N(N-1)/2`may seem bad, *being O(N^2)*, it is unimaginably better than the *O(N!)* naive parsing algorithm!



### Trace `parse example acceptable`

Below is a rendering of the permutation parse tree with partial computations. The symbols `<...>` represent edge transition which correspond to the enclosed combinator. The `[...]` symbols spanning two lines represent a node's parser state.

```
              [ , ,  ]
              ["ccba"]
            /     |   \
          /       |     \
        /         |       \
      /           |         \
   ( a )        ( b*)     ( c+)
     |            |          |
   [ERR]       [ , ,  ]  [ , ,cc]
   [ERR]       ["ccba"]  ["ba"  ]
   /   \        /   \      /   \
( b*) ( c+) ( a ) ( c+) ( a ) ( b*)
  |     |     |     |     |     |
[ERR] [ERR] [ERR] [,,cc][ERR] [ ,b,cc]
[ERR] [ERR] [ERR] ["ba"][ERR] ["a"   ]
  |     |     |     |     |     |
( c+) ( b*) ( c+) ( a ) ( b*) ( a )
  |     |     |     |     |     |
[ERR] [ERR] [ERR] [ERR] [ERR] [a,b,cc]
[ERR] [ERR] [ERR] [ERR] [ERR] [""    ]
                                ^
                                |
SUCCESS ------------------------+

>>> parse example acceptable
Right ('a', "b", "cc")

>>> parse example acceptable == Right success
True
```


### Trace `parse example nonhalting`

The same `6` regular expressions will accepted by the permutation parser `example`.

```
             [ , ,  ]
             ["ccbacbcb"]
            /     |      \
          /       |        \
        /         |          \
      /           |            \
   ( a )        ( b*)         ( c+)
     |            |             |
   [ERR]        [ , ,cc]      [ , ,cc]
   [ERR]        ["ccbacbcb"]  ["bacbcb"]
   /   \        /   \          /   \
( b*) ( c+) ( a ) ( c+)     ( a ) ( b*)
  |     |     |     |         |     |
[ERR] [ERR] [ERR] [,,cc]    [ERR] [ ,b,cc]
[ERR] [ERR] [ERR] ["bacbcb"][ERR] ["acbcb"]
  |     |     |     |         |     |
( c+) ( b*) ( c+) ( a )     ( b*) ( a )
  |     |     |     |         |     |
[ERR] [ERR] [ERR] [ERR]     [ERR] [a,b,cc]
[ERR] [ERR] [ERR] [ERR]     [ERR] ["cbcb"]
                                ^
                                |
SUCCESS ------------------------+

>>> parse example nonhalting`
Right ('a', "b", "cc")

>>> parse example nonhalting == Right failure
False
```

Here we can see that the permutation parser `example` "succeeds" in parsing `nonhalting`, but the result leaves in the unconsumed suffix many of the stream symbols the proposed combinators `permMany` and `permSome` were desired to match. This occurs because the permutation parsing algorithm dismisses a component combinator after a match, and never again considers it for the remainder of the input stream parsing. To modify the algorithm to support the desired semantics of `permMany` and `permSome` we would need to construct a new conputation tree which, as the nondeterministic computation progresses, *preserves* all composite combinators which are either `permMany` or `permSome` definitions. Let's visualize what such a conputation tree would look like for `example`.


### Modified computational tree of`example`


```
                             [___]
                         /     |     \
                     /         |        \
                 /             |           \
             /                 |              \
       ( a )                  ( b*)
         |                      |         
       [a__].                   [_b?]<------+
       /   \                   /     \      |
    ( b*) ( c+)            ( a )     ( c+)( b*)
      |     |                |         |    |
 +->[ab?] [a?c]<-+   +---> [ab?]<-(a)-[_bc]-+
 |    |     |    |   |    /  ^  \      
(b*)(c+)   (b*)(c+)(c+)(b*) (b*)(c+)
 |    V     V    |   |   V   |   V   
 +--[abc] [abc]--+   +-[abc] +-[abc]-
      |     |            |       |
    [END] [END]        [END]   [END]
```




[DOI-0]: https://doi.org/10.1017/S0956796804005143

[fun-pearl ]: https://wiki.haskell.org/Research_papers/Functional_pearls#Online

[gh-mrkkrp ]: https://github.com/mrkkrp
[gh-feature]: https://github.com/mrkkrp/parser-combinators/issues/39

[mp-package]: https://hackage.haskell.org/package/megaparsec
[mp-v6.0.0 ]: https://hackage.haskell.org/package/megaparsec-6.0.0

[pc-package]: https://hackage.haskell.org/package/parser-combinators
[pc-module ]: https://hackage.haskell.org/package/parser-combinators/docs/Control-Applicative-Permutations.html
[pc-v0.2.0 ]: https://hackage.haskell.org/package/parser-combinators-0.2.0
[pc-rev-dep]: https://packdeps.haskellers.com/reverse/parser-combinators

[wiki-CTL  ]: https://en.wikipedia.org/wiki/Computation_tree_logic
