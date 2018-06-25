## [v0.4.0.2](https://github.com/diagrams/diagrams-haddock/tree/v0.4.0.2) (2018-06-25)

A bunch of upper bound updates etc.:

- Allow `lens-4.16`, `Cabal-2.2` `QuickCheck-2.11`,
  `haskell-src-exts-1.20`, `tasty-1.1`, `tasty-quickcheck-0.10`,
  `ansi-terminal-0.8`
- Updates to work with several different versions of `Cabal`

## [v0.4.0.1](https://github.com/diagrams/diagrams-haddock/tree/v0.4.0.1) (2016-11-28)

- Allow `haskell-src-exts-1.19` and `haskell-src-exts-simple-1.19`

## [v0.4](https://github.com/diagrams/diagrams-haddock/tree/v0.4) (2016-10-26)

- Build with `ghc-8.0`
- Improved error message when `setup-config` can't be read
- Fix bug in CPP handling
- Replace `lucid-svg` with `svg-builder`
- Allow `diagrams-svg-1.4`
- Require `diagrams-lib-1.4`
- Allow `haskell-src-exts-1.18`
- Allow `diagrams-builder-0.8`
- Allow `lens-4.15`

## [v0.3.0.10](https://github.com/diagrams/diagrams-haddock/tree/v0.3.0.10) (2015-12-06)

- allow `haskell-src-exts-1.17.*`

## [v0.3.0.9](https://github.com/diagrams/diagrams-haddock/tree/v0.3.0.9) (2015-11-20)

- Allow `lucid-svg-0.6`
- Fix broken v0.3.0.8

## [v0.3.0.8](https://github.com/diagrams/diagrams-haddock/tree/v0.3.0.8) (2015-11-15)

- Don't use this version.

## [v0.3.0.7](https://github.com/diagrams/diagrams-haddock/tree/v0.3.0.7) (2015-09-22)

- Allow `lens-4.13`, `linear-1.20`, and `tasty-0.11`

[Full Changelog](https://github.com/diagrams/diagrams-haddock/compare/v0.3.0.6...v0.3.0.7)

## [v0.3.0.6](https://github.com/diagrams/diagrams-haddock/tree/v0.3.0.6) (2015-07-19)

[Full Changelog](https://github.com/diagrams/diagrams-haddock/compare/v0.3.0.5...v0.3.0.6)

0.3.0.5 (10 July 2015)
-----------------------

- Allow `lucid-svg-0.5

## [v0.3.0.4](https://github.com/diagrams/diagrams-haddock/tree/v0.3.0.4) (2015-05-26)

[Full Changelog](https://github.com/diagrams/diagrams-haddock/compare/v0.3.0.3...v0.3.0.4)

0.3.0.3 (29 April 2015)
-----------------------

- Allow `diagrams-svg-1.3.1

0.3.0.2 (29 April 2015)
-----------------------

- allow QuickCheck-2.8 in test suite

0.3.0.1 (20 April 2015)
-----------------------

- ghc-7.10 compatability

0.3 (19 April 2015)
-------------------

- Allow `diagrams-builder-0.7`, `diagrams-lib-1.3`, and `diagrams-svg-1.3`

0.2.2.14 (2 April 2015)
-----------------------

 - allow `lens-4.9`
 - allow `vector-space-0.10`

0.2.2.13 (13 Jan 2015)
----------------------

- Allow `vector-space-0.9`
- Allow `lens-4.7`
- Allow `Cabal-1.22`
- Allow `tasty-1.10`

0.2.2.12 (20 November 2014)
---------------------------

- Allow `lens-4.6`
- Allow `text-1.2`
- Allow and require `haskell-src-exts-1.16`


0.2.2.11 (22 August 2014)
-------------------------

  - Allow lens-4.4

0.2.2.10 (5 June 2014)
----------------------

  - allow `diagrams-builder-0.6`

0.2.2.9 (2 June 2014)
---------------------

  - allow `lens-4.2` in the test suite as well

0.2.2.8 (28 May 2014)
---------------------

  - allow `diagrams-lib-1.2`, `diagrams-core-1.2`
  - allow `cabal-1.20`
  - allow `mtl-2.2`
  - allow `lens-4.2`

0.2.2.7 (16 April 2014)
-----------------------

  - allow `haskell-src-exts-1.15` in test suite

0.2.2.6 (15 April 2014)
-----------------------

  - allow `haskell-src-exts-1.15`

0.2.2.5 (7 April 2014)
----------------------

  - switch to tasty framework for tests, and allow `QuickCheck-2.7`

0.2.2.4 (20 March 2014)
----------------------

  - allow `lens-4.1`

0.2.2.3 (8 March 2014)
----------------------

    - allow `lens-4.0` in test suite too

0.2.2.2 (6 March 2014)
----------------------

    - allow diagrams-lib-1.1

0.2.2.1 (12 February 2014)
--------------------------

    - allow lens-4.0

0.2.2 (27 January 2014)
-----------------------

    - require diagrams-builder-0.5

0.2.1.6 (15 January 2014)
-------------------------

    - allow text-1.1

0.2.1.5 (31 December 2013)
--------------------------

    - allow text-1.0
	- allow lens-3.10 for the test suite

0.2.1.4 (26 November 2013)
--------------------------

    - require diagrams-builder-0.4.2, allowing the use of IO actions
      producing diagrams

0.2.1.3 (24 November 2013)
--------------------------

    - update `README`: `extra-doc-files` field in `.cabal` is now supported
      by Hackage
    - allow diagrams-lib-1.0 and diagrams-svg-1.0

0.2.1.2 (14 November 2013)
--------------------------

    - allow base-4.7
    - allow lens-3.10

0.2.1.1 (14 October 2013)
-------------------------

    - bug fix: correctly handle hsenv environments with no name

0.2.1 (11 September 2013)
-------------------------

    - prettier progress output and error logging
    - allow Cabal-1.18
    - require diagrams-svg >= 0.8.0.1

0.2 (2 September 2013)
----------------------

    - Take active hsenv into account when looking for distdir (closes #18)
    - add an option to generate data URIs instead of external SVGs
    - base generated diagram file names on module name + diagram name,
      so diagrams with the same name in different files no longer
      clobber each other

0.1.2.0 (1 September 2013)
--------------------------

    - part before # is now optional when writing a new diagram URL

0.1.1.2 (22 August 2013)
------------------------

    - update for haskell-src-exts 1.14
    - 'extra-html-files' is now called 'extra-doc-files'

0.1.1.1 (1 August 2013)
-----------------------

  * allow `diagrams-lib-0.7` and `diagrams-svg-0.7`

0.1.1.0 (23 June 2013)
----------------------

  * Add -d option for specifying dist dir

0.1.0.1 (27 March 2013)
-----------------------

  * bump upper bounds:
      - allow lens-3.9
      - allow QuickCheck-2.6

0.1.0.0 (23 March 2013)
-----------------------

Initial release!
