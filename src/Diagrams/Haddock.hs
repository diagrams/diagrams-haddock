{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Haddock
-- Copyright   :  (c) 2013 diagrams-haddock team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Include inline diagrams code in Haddock documentation!  For
-- example, here is a green circle:
--
-- <<greenCircle.svg#diagram=greenCircle&width=200>>
--
-- which was literally produced by this code:
--
-- > greenCircle = circle 1
-- >             # fc green # pad 1.1
--
-----------------------------------------------------------------------------
module Diagrams.Haddock
    ( -- * Diagram URLs
      -- $urls

      DiagramURL(..)
    , displayDiagramURL
    , parseDiagramURL
    , parseKeyValPair
    , maybeParseDiagramURL

    , parseDiagramURLs
    , displayDiagramURLs

      -- * Comments
      -- $comments

    , CommentWithURLs(..)
    , getDiagramNames
    , explodeComment
    , collapseComment
    , coalesceComments

      -- * Code blocks
      -- $codeblocks

    , CodeBlock(..)
    , makeCodeBlock
    , collectBindings
    , extractCodeBlocks

      -- * Modules
      -- $modules

    , ParsedModule(..)
    , parseModule

      -- * Diagram compilation
      -- $diagrams

    , compileDiagram
    , compileDiagrams
    , processHaddockDiagrams

    ) where

import           Prelude                         hiding (writeFile)
import qualified System.IO.Cautious              as Cautiously

import           Control.Applicative             hiding (many, (<|>))
import           Control.Arrow                   (first, (&&&), (***))
import           Control.Lens                    hiding ((<.>))
import           Control.Monad.Writer
import qualified Data.ByteString.Lazy            as BS
import           Data.Char                       (isSpace)
import           Data.Either                     (lefts, rights)
import           Data.Function                   (on)
import           Data.List                       (groupBy, intercalate,
                                                  isPrefixOf)
import           Data.List.Split                 (dropBlanks, dropDelims, split,
                                                  whenElt)
import qualified Data.Map                        as M
import           Data.Maybe                      (catMaybes, mapMaybe)
import qualified Data.Set                        as S
import           Data.VectorSpace                (zeroV)
import           Language.Haskell.Exts.Annotated hiding (loc, parseModule)
import qualified Language.Haskell.Exts.Annotated as HSE
import           Language.Preprocessor.Cpphs
import           System.Directory                (copyFile,
                                                  createDirectoryIfMissing,
                                                  doesFileExist)
import           System.FilePath                 ((<.>), (</>))
import qualified System.IO.Strict                as Strict
import           Text.Blaze.Svg.Renderer.Utf8    (renderSvg)
import           Text.Parsec
import qualified Text.Parsec                     as P
import           Text.Parsec.String

import           Diagrams.Backend.SVG            (Options (..), SVG (..))
import           Diagrams.Builder                (BuildResult (..),
                                                  buildDiagram,
                                                  hashedRegenerate,
                                                  ppInterpError)
import           Diagrams.TwoD.Size              (mkSizeSpec)

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

showParseFailure :: SrcLoc -> String -> String
showParseFailure loc err = unlines [ prettyPrint loc, err ]

newtype CollectErrors a = CE { unCE :: Writer [String] a }
  deriving (Functor, Applicative, Monad, MonadWriter [String])

failWith :: String -> CollectErrors (Maybe a)
failWith err = tell [err] >> return Nothing

runCE :: CollectErrors a -> (a, [String])
runCE = runWriter . unCE

------------------------------------------------------------
-- Diagram URLs
------------------------------------------------------------

-- $urls
-- Haddock supports inline links to images with the syntax
-- @\<\<URL\>\>@.  To indicate an image which should be automatically
-- generated from some diagrams code, we use the special syntax
-- @\<\<URL#diagram=name&key1=val1&key2=val2&...\>\>@.  The point is
-- that everything following the @#@ will be ignored by browsers, but
-- we can use it to indicate to diagrams-haddock the name of the
-- diagram to be rendered along with options such as size.

-- | An abstract representation of inline Haddock image URLs with
--   diagrams tags, like @\<\<URL#diagram=name&width=100\>\>@.
data DiagramURL = DiagramURL
                  { _diagramURL  :: String
                  , _diagramName :: String
                  , _diagramOpts :: M.Map String String
                  }
  deriving (Show, Eq)

makeLenses ''DiagramURL

-- | Display a diagram URL in the format @\<\<URL#diagram=name&key=val&...\>\>@.
displayDiagramURL :: DiagramURL -> String
displayDiagramURL d = "<<" ++ d ^. diagramURL ++ "#" ++ opts ++ ">>"
  where
    opts = intercalate "&"
         . map displayOpt
         . (("diagram", d ^. diagramName) :)
         . M.assocs
         $ d ^. diagramOpts
    displayOpt (k,v) = k ++ "=" ++ v

-- | Parse things of the form @\<\<URL#diagram=name&key=val&...\>\>@.
parseDiagramURL :: Parser DiagramURL
parseDiagramURL =
  DiagramURL
  <$> (string "<<" *> many1 (noneOf "#>"))
  <*> (char '#' *> string "diagram=" *> many1 (noneOf "&>"))
  <*> ((M.fromList <$> many parseKeyValPair) <* string ">>")

-- | Parse a key/value pair of the form @&key=val@.
parseKeyValPair :: Parser (String,String)
parseKeyValPair =
  char '&' *>
  ((,) <$> (many1 (noneOf "&>=") <* char '=') <*> many1 (noneOf "&>="))

-- | Parse a diagram URL /or/ a single character which is not the
--   start of a diagram URL.
maybeParseDiagramURL :: Parser (Either Char DiagramURL)
maybeParseDiagramURL =
      Right <$> try parseDiagramURL
  <|> Left  <$> anyChar

-- | Decompose a string into a parsed form with explicitly represented
--   diagram URLs interspersed with other content.
parseDiagramURLs :: Parser [Either String DiagramURL]
parseDiagramURLs = condenseLefts <$> many maybeParseDiagramURL
  where
    condenseLefts :: [Either a b] -> [Either [a] b]
    condenseLefts [] = []
    condenseLefts (Right a : xs) = Right a : condenseLefts xs
    condenseLefts xs = Left (lefts ls) : condenseLefts xs'
      where (ls,xs') = span isLeft xs
            isLeft (Left {}) = True
            isLeft _         = False

-- | Serialize a parsed comment with diagram URLs back into a String.
displayDiagramURLs :: [Either String DiagramURL] -> String
displayDiagramURLs = concatMap (either id displayDiagramURL)

------------------------------------------------------------
-- Comments
------------------------------------------------------------

-- $comments
-- The @haskell-src-exts@ package defines a 'Comment' type for
-- representing comments.  Here we define a type to represent parsed
-- comments with explicitly represented diagram URLs
-- ('explodeComment'), so that the diagram URLs can be easily
-- extracted ('getDiagramNames') and manipulated before being
-- serialized back into a 'Comment' ('collapseComment').

-- | The @CommentWithURLs@ type represents a Haddock comment
--   potentially containing diagrams URLs, but with the URLs separated
--   out so they are easy to query and modify; ultimately the whole
--   thing can be turned back into a 'Comment'.
data CommentWithURLs
    = CommentWithURLs
      { _originalComment :: Comment
      , _diagramURLs     :: [Either String DiagramURL]
      }
  deriving (Show, Eq)

makeLensesFor [("_diagramURLs", "diagramURLs")] ''CommentWithURLs

-- | Get the names of all diagrams referenced in the given comment.
getDiagramNames :: CommentWithURLs -> S.Set String
getDiagramNames = S.fromList . map (view diagramName) . rights . view diagramURLs

-- | \"Explode\" the content of a comment to expose the diagram URLs
--   for easy processing.
explodeComment :: Comment -> CommentWithURLs
explodeComment c@(Comment _ _ s) =
  case P.parse parseDiagramURLs "" s of
    Left _     -> error "This case can never happen; see prop_parseDiagramURLs_succeeds"
    Right urls -> CommentWithURLs c urls

-- | \"Collapse\" a parsed comment back down into a normal
--   comment. Exploding and then collapsing a comment yields the
--   original comment; that is, @collapseComment . explodeComment ===
--   id@.
collapseComment :: CommentWithURLs -> Comment
collapseComment (CommentWithURLs (Comment b s _) urls)
  = Comment b s (displayDiagramURLs urls)

-- | Given a series of comments, return a list of their contents,
--   coalescing blocks of adjacent single-line comments into one
--   String.  Each string will be paired with the number of the line
--   on which it begins.
coalesceComments :: [Comment] -> [(String, Int)]
coalesceComments
  = map (unlines . map getComment &&& commentLine . head)

    -- discard no longer needed numbers
  . map (map fst)

    -- group consecutive runs
  . concatMap (groupBy ((==) `on` snd))

    -- subtract consecutive numbers so runs show up as repeats
    -- e.g.  L1, L2, L3, L6, L7, L9  -->  0,0,0,2,2,3
  . map (zipWith (\i c -> (c, commentLine c - i)) [1..])

    -- explode out each multi-line comment into its own singleton list,
    -- which will be unaffected by the above shenanigans
  . concatMap (\xs -> if isMultiLine (head xs) then map (:[]) xs else [xs])

    -- group multi + single line comments together
  . groupBy ((==) `on` isMultiLine)

  where
    isMultiLine (Comment b _ _) = b
    getComment  (Comment _ _ c) = c
    commentLine (Comment _ s _) = srcSpanStartLine s

    -- Argh, I really wish the split package supported splitting on a
    -- predicate over adjacent elements!  That would make the above
    -- soooo much easier.

------------------------------------------------------------
-- Code blocks
------------------------------------------------------------

-- $codeblocks
-- A code block represents some portion of a comment set off by bird
-- tracks.  We also collect a list of the names bound in each code
-- block, in order to decide which code blocks contain expressions
-- representing diagrams that are to be rendered.

-- | A @CodeBlock@ represents a portion of a comment which is a valid
--   code block (set off by > bird tracks).  It also caches the list
--   of bindings present in the code block.
data CodeBlock
    = CodeBlock
      { _codeBlockCode     :: String
      , _codeBlockBindings :: [String]
      }
  deriving (Show, Eq)

makeLenses ''CodeBlock

-- | Given a @String@ representing a code block, /i.e./ valid Haskell
--   code with any bird tracks already stripped off, along with its
--   beginning line number (and the name of the file from which it was
--   taken), attempt to parse it, extract the list of bindings
--   present, and construct a 'CodeBlock' value.
makeCodeBlock :: FilePath -> (String,Int) -> CollectErrors (Maybe CodeBlock)
makeCodeBlock file (s,l) =
  case HSE.parseFileContentsWithMode parseMode s of
    ParseOk m           -> return . Just $ CodeBlock s (collectBindings m)
    ParseFailed loc err -> failWith . unlines $
      [ file ++ ": " ++ show l ++ ": Warning: could not parse code block:" ]
      ++
      showBlock s
      ++
      [ "Error was:" ]
      ++
      (indent 2 . lines $ showParseFailure loc err)
  where
    parseMode = defaultParseMode
                { fixities = Nothing
                , extensions = MultiParamTypeClasses : haskell2010
                }
    indent n  = map (replicate n ' ' ++)
    showBlock b
      | length ls > 5 = indent 2 (take 4 ls ++ ["..."])
      | otherwise     = indent 2 ls
      where ls = lines b


-- | Collect the list of names bound in a module.
collectBindings :: Module l -> [String]
collectBindings (Module _ _ _ _ decls) = mapMaybe getBinding decls
collectBindings _ = []

getBinding :: Decl l -> Maybe String
getBinding (FunBind _ [])                     = Nothing
getBinding (FunBind _ (Match _ nm _ _ _ : _)) = Just $ getName nm
getBinding (PatBind _ (PVar _ nm) _ _ _)      = Just $ getName nm
getBinding _                                  = Nothing

getName :: Name l -> String
getName (Ident _ s)  = s
getName (Symbol _ s) = s

-- | From a @String@ representing a comment (along with its beginning
--   line number, and the name of the file it came from, for error
--   reporting purposes), extract all the code blocks (consecutive
--   lines beginning with bird tracks), and error messages for code
--   blocks that fail to parse.
extractCodeBlocks :: FilePath -> (String,Int) -> CollectErrors [CodeBlock]
extractCodeBlocks file (s,l)
  = fmap catMaybes
  . mapM (makeCodeBlock file . (unlines***head) . unzip . (map.first) (drop 2 . dropWhile isSpace))
  . (split . dropBlanks . dropDelims $ whenElt (not . isBird . fst))
  . flip zip [l ..]
  . lines
  $ s
  where
    isBird = ("> " `isPrefixOf`) . dropWhile isSpace

------------------------------------------------------------
-- Modules
------------------------------------------------------------

-- $modules
-- A representation for parsed modules including their code blocks and
-- diagram URLs.

-- | A @ParsedModule@ value contains a haskell-src-exts parsed module,
--   a list of exploded comments, and a list of code blocks which
--   contain bindings referenced in diagrams URLs.
data ParsedModule = ParsedModule
                    { _pmModule   :: Module SrcSpanInfo
                    , _pmComments :: [CommentWithURLs]
                    , _pmCode     :: [CodeBlock]
                    }

makeLensesFor [("_pmCode", "pmCode")] ''ParsedModule

-- | Turn the contents of a @.hs@ file into a 'ParsedModule'.
parseModule :: FilePath -> String -> CollectErrors (Maybe ParsedModule)
parseModule file src =
  case HSE.parseFileContentsWithComments parseMode src of
    ParseFailed loc err -> failWith $ showParseFailure loc err
    ParseOk (m, cs)     -> do
      allBlocks <- fmap concat
                   . mapM (extractCodeBlocks file)
                   . coalesceComments
                   $ cs
      let cs'       = map explodeComment cs
          diaNames  = S.unions . map getDiagramNames $ cs'
          blocks    = filter (any (`S.member` diaNames) . view codeBlockBindings)
                             allBlocks
      return . Just $ ParsedModule m cs' blocks
  where
    parseMode = defaultParseMode
                { fixities      = Nothing
                , parseFilename = file
                , extensions    = MultiParamTypeClasses : haskell2010
                }

------------------------------------------------------------
-- Diagrams
------------------------------------------------------------

-- $diagrams
-- This section contains all the functions which actually interface
-- with diagrams-builder in order to compile diagrams referenced from
-- URLs.

-- | Given a directory for cached diagrams and a directory for
--   outputting final diagrams, and all the relevant code blocks,
--   compile the diagram referenced by a single URL, returning a new
--   URL updated to point to the location of the generated diagram.
--   Also return a @Bool@ indicating whether the URL changed.
--
--   In particular, the diagram will be output to @outDir/name.svg@,
--   where @outDir@ is the second argument to @compileDiagram@, and
--   @name@ is the name of the diagram.  The updated URL will refer to
--   @name.svg@ (/not/ @outDir/name.svg@), under the assumption that
--   the contents of @outDir@ will be copied into the Haddock output
--   directory.  If for some reason you would like this scheme to be
--   more flexible/configurable, just yell.
compileDiagram :: FilePath   -- ^ cache directory
               -> FilePath   -- ^ output directory
               -> [CodeBlock] -> DiagramURL -> IO (DiagramURL, Bool)
compileDiagram cacheDir outputDir code url = do
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True cacheDir

  let baseFile = (url ^. diagramName) <.> "svg"
      outFile  = outputDir </> baseFile

      w = read <$> M.lookup "width" (url ^. diagramOpts)
      h = read <$> M.lookup "height" (url ^. diagramOpts)

      oldURL = (url, False)
      newURL = (url & diagramURL .~ baseFile, baseFile /= url^.diagramURL)

  res <- buildDiagram
           SVG
           zeroV
           (SVGOptions (mkSizeSpec w h))
           (map (view codeBlockCode) code)
           (url ^. diagramName)
           []
           [ "Diagrams.Backend.SVG" ]
           (hashedRegenerate (\_ opts -> opts) cacheDir)

  case res of
    -- XXX incorporate these into error reporting framework instead of printing
    ParseErr err    -> do
      putStrLn ("Parse error:")
      putStrLn err
      return oldURL
    InterpErr ierr  -> do
      putStrLn ("Interpreter error:")
      putStrLn (ppInterpError ierr)
      return oldURL
    Skipped hash    -> do
      copyFile (mkCached hash) outFile
      return newURL
    OK hash svg     -> do
      let cached = mkCached hash
      BS.writeFile cached (renderSvg svg)
      copyFile cached outFile
      return newURL

 where
   mkCached base = cacheDir </> base <.> "svg"

-- | Compile all the diagrams referenced in an entire module.
compileDiagrams :: FilePath      -- ^ cache directory
                -> FilePath      -- ^ output directory
                -> ParsedModule
                -> [Either String DiagramURL] -> IO ([Either String DiagramURL], Bool)
compileDiagrams cacheDir outputDir m urls = do
  urls' <- urls & (traverse . _Right)
                %%~ compileDiagram cacheDir outputDir (m^.pmCode)
  let changed = orOf (traverse . _Right . _2) urls'
  return (urls' & (traverse . _Right) %~ fst, changed)

-- | Read a file, compile all the referenced diagrams, and update all
--   the diagram URLs to refer to the proper image files.  Note, this
--   /overwrites/ the file, so it's recommended to only do this on
--   files that are under version control, so you can compare the two
--   versions and roll back if 'processHaddockDiagrams' does something
--   horrible.
--
--   Returns a list of warnings and/or errors.
processHaddockDiagrams
  :: FilePath   -- ^ cache directory
  -> FilePath   -- ^ output directory
  -> [FilePath] -- ^ include directories
  -> FilePath   -- ^ file to be processed
  -> IO [String]
processHaddockDiagrams cacheDir outputDir includeDirs file = do
  e   <- doesFileExist file
  case e of
    False -> return ["Error: " ++ file ++ " not found."]
    True  -> do
      src <- Strict.readFile file
      r <- go src
      case r of
        (Nothing, msgs) -> return msgs
        (Just m , msgs) ->
          case P.parse parseDiagramURLs "" src of
            Left _     ->
              error "This case can never happen; see prop_parseDiagramURLs_succeeds"
            Right urls -> do
              (urls', changed) <- compileDiagrams cacheDir outputDir m urls
              let src' = displayDiagramURLs urls'
              when changed $ Cautiously.writeFile file src'
              return msgs
  where
    go src =
      case runCE (parseModule file src) of
        r@(Nothing, msgs) -> if any (("Parse error: #" `elem`) . lines) msgs
                             then runCpp src >>= return . runCE . parseModule file
                             else return r
        r -> return r
    runCpp s = runCpphs opts file s
    opts = defaultCpphsOptions
         { includes = includeDirs
         , boolopts = defaultBoolOptions { hashline = False }
         }
