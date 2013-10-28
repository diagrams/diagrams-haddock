# diagrams-haddock

`diagrams-haddock` is a preprocessor which allows embedding images
generated using the [diagrams
framework](http://projects.haskell.org/diagrams/) within Haddock
documentation.  The code to generate images is embedded directly
within the source file itself (and the code can be included in the
Haddock output or not, as you wish).  `diagrams-haddock` takes care of
generating SVG images and linking them into the Haddock output.

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

## An important caveat

`diagrams-haddock` modifies files *in place*!  While we have worked
hard to ensure that it cannot make catastrophic changes to your files,
you would be wise to **only run `diagrams-haddock` on files under
version control** so you can easily examine and (if necessary) undo
the changes it makes.  (Of course, being a conscientious developer,
you would never work with source files not under version control,
right?)

## Adding diagrams to source files

Haddock supports inline links to images with the syntax `<<URL>>`.  To
indicate an image which should be automatically generated from some
diagrams code, use the special syntax
`<<URL#diagram=name&key1=val1&key2=val2&...>>`.  The URL will be
automatically filled in by `diagrams-haddock`, so when you first
create an inline image placeholder you can simply omit it (or put any
arbitrary text in its place).  For example, you might write

    <<#diagram=mySquare&width=200&height=300>>

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

## Code block dependency analysis

`diagrams-haddock` does a simple dependency analysis to determine
which code blocks should be in scope while compiling each diagram.
First, it locates a code block containing a binding for the requested
diagram name.  Then, it pulls in any code blocks containing bindings
for identifiers referenced by this code block, and so on transitively.
(Note that this analysis is overly simplistic and does not take things
like shadowing into account; this may sometimes cause additional code
blocks to be included which would not be included with a more careful
analysis.)

This has a few implications.  First, code blocks containing irrelevant
bindings will not be considered.  It is common to have code blocks
which are intended simply to show some example code---they may not
even be valid Haskell.  However, as long as such code blocks do not
contain any bindings of names used by a diagram, they will be ignored.
For example:

``` haskell
-- The algorithm works by doing the equivalent of
--
-- > rep = uncurry replicate
-- >
-- > algo = map rep . zip [1..]
--
-- as illustrated below:
--
-- <<#diagram=algoIllustration&width=400>>
--
-- > algoIllustration = ...
```

The first code block shown above (beginning `rep = ...`) contains some
bindings, but none of those bindings are referenced by any diagram
URLs, so the code block is ignored.

Another convenient implication is that supporting code can be put in
separate code blocks and even shared between diagrams.  For example:

``` haskell
-- > makeitblue d = d # fc blue # lc blue
--
-- Here is a blue circle:
--
-- <<#diagram=blueC&width=200>>
--
-- > blueC = circle 1 # makeitblue
--
-- And here is a blue square:
--
-- <<#diagram=blueS&width=200>>
--
-- > blueS = square 1 # makeitblue
```

This also means that diagrams are recompiled only when necessary.  For
example, if the definition of `blueC` is changed, only `blueC` will be
recompiled.  If the definition of `makeitblue` is changed, both
`blueC` and `blueS` will be recompiled.

## Invoking diagrams-haddock

Invoking the `diagrams-haddock` tool is simple: just give it a
list of targets, like so:

```
diagrams-haddock foo.hs baz/bar.lhs ~/src/some-cabal-directory
```

* For file targets, `diagrams-haddock` simply processes the given file.

* Directory targets are assumed to contain Cabal packages, which
  themselves contain a library. `diagrams-haddock` then finds and
  processes the source files corresponding to all modules exported by
  the library.  (Note that `diagrams-haddock` does not currently run on
  unexported modules or on the source code for executables, but if you
  have a use case for either, just file a [feature
  request](https://github.com/diagrams/diagrams-haddock/issues); they
  shouldn't be too hard to add.)

Also, if you simply invoke `diagrams-haddock` with no targets, it will
process the Cabal package in the current directory.

`diagrams-haddock` also takes a few command-line options which can be
used to customize its behavior:

* `-c`, `--cachedir`: When diagrams are compiled, their source code is
  hashed and the output image stored in a file like `068fe.......342.svg`,
  with the value of the hash as the name of the file.  This way, if
  the source code for a diagram has not changed in between invocations
  of `diagrams-haddock`, it does not need to be recompiled.  This
  option lets you specify the directory where such cached SVG files
  should be stored; the default is `.diagrams-cache`.

* `-o`, `--outputdir`: This is the directory into which the final
  output images will be produced.  The default is `diagrams`.

* `-d`, `--distdir`: When building diagrams for a cabal package, this
  is the directory in which `diagrams-haddock` should look for the
  `setup-config` file (*i.e.* the output of `cabal configure`).  An
  explicit value for this flag takes precedence; next,
  `diagrams-haddock` checks whether there is an active
  [hsenv](http://hackage.haskell.org/package/hsenv) environment, and
  if so uses `dist_<hsenv name>`; otherwise, it defaults to using
  `dist`.

* `-i`, `--includedirs`: `diagrams-haddock` does its best to process
  files with CPP directives, even extracting information about where
  to find `#include`s from the `.cabal` file, but sometimes it might
  need a little help.  This option lets you specify additional
  directories in which `diagrams-haddock` should look when searching
  for `#include`d files.

* `--cppdefines`: likewise, this option allows you to specify
  additional names that should be `#define`d when CPP is run.

* `--dataURIs`: embed the generated SVG images directly in the source
  code with [data URIs](http://en.wikipedia.org/wiki/Data_URI_scheme‎)
  (the default is to generate external SVG files and link to them).
  See the section below for a discussion of the tradeoffs involved.

* `-q`, `--quiet`: `diagrams-haddock` normally prints some logging
  information to indicate what it is doing; this option silences the
  output.

## Workflow and Cabal setup

There are two ways one may include generated SVG images with your
documentation: as data URIs, or as external images.  The two options
are discussed below, along with pros and cons of each.  Note that in
either case, consumers of your library (including Hackage itself) do
*not* need to have `diagrams-haddock` installed in order to build your
documentation.

### Using data URIs

If you pass the `--dataURIs` option to `diagrams-haddock`, any
generated images will be embedded directly in your source file (and
hence also in the HTML ultimately produced by `haddock`) as
[data URIs](http://en.wikipedia.org/wiki/Data_URI_scheme‎).  To use
this method,

1. Include inline diagrams code and URLs in your source code.
2. Run `diagrams-haddock --dataURIs`.
3. Commit the resulting URL changes to your source files.

The benefit of this scheme is that there are no extra files to deal
with, and no need to alter your `.cabal` file in any way.  The
downside is that it significantly bloats your source code, and may
make it extremely inconvenient to edit without some sort of tool
support (*e.g.* an editor that can "collapse" certain sections of the
source file).

### Using external images

By default, `diagrams-haddock` generates external SVG image files.  This
makes for a much less invasive changes to your source files, but
requires some extra work to manage the extra files.  To use this method,

1. Include inline diagrams code and URLs in your source code.
2. Run `diagrams-haddock`.
3. Commit the resulting URL changes to your source files *and* the produced SVG files.
4. Arrange to have the SVG files installed along with your package's
   Haddock documentation (more on this below).

The generated SVG files need to be copied alongside the generated
Haddock documentation.  There are two good ways to accomplish this:

1.  As of version 1.18, The `cabal` tool has acquired an
    `extra-doc-files` field (see
    https://github.com/haskell/cabal/pull/1182 and
    https://github.com/haskell/cabal/pull/1427), specifying files
    which should be copied in alongside generated Haddock
    documentation.  So you could simply write something like

    ```
    extra-doc-files: diagrams/*.svg
    ```

    in your `.cabal` file.  However, as of this writing (October
    2013), Hackage is not building packages with `cabal-1.18` (see
    https://github.com/haskell/hackage-server/issues/140).  So this is
    currently a good option only if you have the latest release of
    `cabal` and don't care about others (including Hackage) being able
    to build your documentation.  However, in the
    hopefully-not-too-distant future (once Hackage switches to
    `cabal-1.18`) this will become the best option.

2.  In the meantime, it is possible to take advantage of `cabal`'s
    system of user hooks to manually copy the images right after the
    Haddock documentation is generated.  Add something like

    ```
    build-type: Custom
    extra-source-files: diagrams/*.svg
    ```

    to your `.cabal` file, and then put something like the following in your
    `Setup.hs`:

    ``` haskell
    import           Data.List                 (isSuffixOf)
    import           Distribution.Simple
    import           Distribution.Simple.Setup (Flag (..), HaddockFlags,
                                                haddockDistPref)
    import           Distribution.Simple.Utils (copyFiles)
    import           Distribution.Text         (display)
    import           Distribution.Verbosity    (normal)
    import           System.Directory          (getDirectoryContents)
    import           System.FilePath           ((</>))

    -- Ugly hack, logic copied from Distribution.Simple.Haddock
    haddockOutputDir :: Package pkg => HaddockFlags -> pkg -> FilePath
    haddockOutputDir flags pkg = destDir
       where
         baseDir = case haddockDistPref flags of
                          NoFlag -> "."
                          Flag x -> x
         destDir = baseDir </> "doc" </> "html" </> display (packageName pkg)

    diagramsDir = "diagrams"

    main :: IO ()
    main = defaultMainWithHooks simpleUserHooks
             { postHaddock = \args flags pkg lbi -> do
                 dias <- filter ("svg" `isSuffixOf`) `fmap` getDirectoryContents diagramsDir
                 copyFiles normal (haddockOutputDir flags pkg)
                   (map (\d -> ("", diagramsDir </> d)) dias)
                 postHaddock simpleUserHooks args flags pkg lbi
             }
    ```

    It may not be pretty, but it works!

## File encodings

For now, `diagrams-haddock` assumes that all `.hs` and `.lhs` files
are encoded using UTF-8.  If you would like to use it with source
files stored using some other encoding, feel free to [file a feature
request](https://github.com/diagrams/diagrams-haddock/issues).

## The diagrams-haddock library

For most use cases, simply using the `diagrams-haddock` executable
should get you what you want.  Note, however, that the internals are
also exposed as a library, making it possible to do all sorts of crazy
stuff you might dream up.  Let us know what you do with it!

## Reporting bugs

Please report any bugs, feature requests, *etc.*, on the [github issue
tracker](https://github.com/diagrams/diagrams-haddock/issues).
