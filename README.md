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
of `mySquare`. XXX finish

## Invoking diagrams-haddock

XXX write about diagrams-haddock command-line tool and its options

## Cabal setup

XXX how to set up your Cabal project to automatically copy the
generated images along with the generated Haddock docs

## The diagrams-haddock library

XXX note that it's also provided as a library so you can do whatever
cool stuff you think up
