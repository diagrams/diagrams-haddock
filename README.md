# diagrams-haddock

`diagrams-haddock` is a preprocessor which allows embedding images in
Haddock documentation generated using the
[diagrams framework](http://projects.haskell.org/diagrams/).  The code
to generate images is embedded directly within the source file itself
(and the code can be included in the Haddock output or not, as you
wish).  `diagrams-haddock` takes care of generating the images and
linking them into the Haddock output.

## Installing

Just `cabal install diagrams-haddock`.  If you have any trouble, ask
in the `#diagrams` freenode IRC channel, or file a ticket on the
[bug tracker](http://github.com/diagrams/diagrams-haddock/issues).

## On the design of `diagrams-haddock`

Before getting into the details of using `diagrams-haddock`, it should
be noted that `diagrams-haddock` has been carefully designed so that
*you only have to maintain a single copy of your source files*.  In
particular, you do *not* have to maintain one copy of your source
files with embedded diagrams code and another copy where the diagrams
code has been replaced by images.  If you find yourself scratching
your head over the quirky ways that `diagrams-haddock` works, now you
will know why.

## Adding diagrams to source files

Haddock supports inline links to images with the syntax
`<<URL>>`.  To indicate an image which should be automatically
generated from some diagrams code, use the special syntax
`<<URL#diagram=name&key1=val1&key2=val2&...>>`.  The URL will be
automatically filled in by `diagrams-haddock`, so when you first
create an inline image placeholder you can put any arbitrary text in
its place.  For example, you might write

    <<dummy#diagram=mySquare&width=200&height=300>>

indicating an image which should be generated using the definition of
`mySquare`, with a maximum width of 200 and maximum height of 300.
(Incidentally, this syntax is used because everything following the
`#` symbol will be ignored by browsers.)

Continuing with the above example, you must also provide a definition
of `mySquare`. You must provide it in a code block, which must be set
off by bird tracks (that is, greater-than symbols followed by at least
one space).  For example,

``` haskell
-- > mySquare = square 1 # fc blue # myTransf
-- > myTransf = rotateBy (1/27)
```

You can choose to have the code block included in the Haddock output
or not, simply by putting it in a Haddock comment or not.  Note that
the code block defining `mySquare` can be anywhere in the same file;
it does not have to be right before or right after the diagram URL
referencing it.

## More on code blocks

There are two important things to realize about the way
`diagrams-haddock` processes code blocks containing diagram
definitions.

The first is that **`diagrams-haddock` only looks at code blocks which
contain a binding** referenced in a diagram URL.  For example:

``` haskell
-- The algorithm works by doing the equivalent of
--
-- > rep = uncurry replicate
-- >
-- > algo = map rep . zip [1..]
--
-- as illustrated below:
--
-- <<dummy#diagram=algoIllustration&width=400>>
--
-- > algoIllustration = ...
```

The first code block shown above (beginning `rep = ...`) contains some
bindings, but none of those bindings are referenced by any diagram
URLs, so the code block is not considered when generating diagrams.

The second is that **all code blocks with referenced bindings are in
scope** when interpreting *each* diagram.  This means that you can
write some generic code which can be reused in the definition of many
different diagrams, *as long as* you include that generic code in the
same code block with some referenced binding.  For example:

``` haskell
-- Here is a blue circle:
--
-- <<dummy#diagram=blueC&width=200>>
--
-- > makeitblue d = d # fc blue # lc blue
-- >
-- > blueC = circle 1 # makeitblue
--
-- And here is a blue square:
--
-- <<dummy#diagram=blueS&width=200>>
--
-- > blueS = square 1 # makeitblue
```

The two code blocks are selected since they both contain referenced
bindings (`blueC` and `blueS`), and their entire combined contents are
in scope while interpreting each diagram.  In particular this means
that the definition of `makeitblue` is available to be used in the
definition of `blueS`.

## Invoking diagrams-haddock

XXX write about diagrams-haddock command-line tool and its options

## Cabal setup

XXX how to set up your Cabal project to automatically copy the
generated images along with the generated Haddock docs

## The diagrams-haddock library

XXX note that it's also provided as a library so you can do whatever
cool stuff you think up
