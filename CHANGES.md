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
