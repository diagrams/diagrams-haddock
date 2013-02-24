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
    , displayModule

      -- * Diagram compilation
      -- $diagrams

    , compileDiagram
    , compileComment
    , compileDiagrams
    , processHaddockDiagrams

    ) where

import           Control.Applicative             hiding (many, (<|>))
import qualified Data.ByteString.Lazy            as BS
import           Data.Char                       (isSpace)
import           Data.Either                     (lefts, rights)
import           Data.Function                   (on)
import           Data.List                       (groupBy, intercalate,
                                                  isPrefixOf)
import           Data.List.Split                 (dropBlanks, dropDelims, split,
                                                  whenElt)
import qualified Data.Map                        as M
import           Data.Maybe                      (mapMaybe)
import qualified Data.Set                        as S
import           Data.VectorSpace                (zeroV)
import           Language.Haskell.Exts.Annotated hiding (parseModule)
import qualified Language.Haskell.Exts.Annotated as HSE
import           System.Directory                (copyFile,
                                                  createDirectoryIfMissing)
import           System.FilePath                 ((<.>), (</>))
import qualified System.IO.Strict                as Strict
import           Text.Blaze.Svg.Renderer.Utf8    (renderSvg)
import           Text.Parsec
import qualified Text.Parsec                     as P
import           Text.Parsec.String

import           Diagrams.Backend.SVG            (Options (..), SVG (..))
import           Diagrams.Builder
import           Diagrams.TwoD.Size              (mkSizeSpec)

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
                  { diagramURL  :: String
                  , diagramName :: String
                  , diagramOpts :: M.Map String String
                  }
  deriving (Show, Eq)

-- | Display a diagram URL in the format @\<\<URL#diagram=name&key=val&...\>\>@.
displayDiagramURL :: DiagramURL -> String
displayDiagramURL d = "<<" ++ diagramURL d ++ "#" ++ opts ++ ">>"
  where
    opts = intercalate "&"
         . map displayOpt
         . (("diagram", diagramName d) :)
         . M.assocs
         $ diagramOpts d
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
      { originalComment :: Comment
      , diagramURLs     :: [Either String DiagramURL]
      }
  deriving (Show, Eq)

-- | Get the names of all diagrams referenced in the given comment.
getDiagramNames :: CommentWithURLs -> S.Set String
getDiagramNames = S.fromList . map diagramName . rights . diagramURLs

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
--   String.
coalesceComments :: [Comment] -> [String]
coalesceComments
  = map unlines
  . (map . map) (getComment . fst)
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
    commentLine (Comment _ span _) = srcSpanStartLine span

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
      { codeBlockCode     :: String
      , codeBlockBindings :: [String]
      }
  deriving (Show, Eq)

-- | Given a @String@ representing a code block, /i.e./ valid Haskell
--   code with any bird tracks already stripped off, attempt to parse
--   it, extract the list of bindings present, and construct a
--   'CodeBlock' value.  If parsing fails, return the error message.
makeCodeBlock :: String -> Either String CodeBlock
makeCodeBlock s =
  case HSE.parseFileContents s of
    ParseFailed _ errStr -> Left errStr
    ParseOk m            -> Right (CodeBlock s (collectBindings m))

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

-- | From a @String@ representing a comment, extract all the code
--   blocks (consecutive lines beginning with bird tracks).
extractCodeBlocks :: String -> [CodeBlock]
extractCodeBlocks
  = rights
  . map (makeCodeBlock . concat . map (drop 2 . dropWhile isSpace))
  . (split . dropBlanks . dropDelims $ whenElt (not . isBird))
  . lines
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
                    { pmModule   :: Module SrcSpanInfo
                    , pmComments :: [CommentWithURLs]
                    , pmCode     :: [CodeBlock]
                    }

-- | Turn the contents of a @.hs@ file into a 'ParsedModule'.
parseModule :: String -> Either String ParsedModule
parseModule src =
  case HSE.parseFileContentsWithComments defaultParseMode src of
    ParseFailed _ errStr -> Left errStr
    ParseOk (m, cs)      ->
      let cs'       = map explodeComment cs
          allBlocks = concatMap extractCodeBlocks
                    . coalesceComments
                    $ cs
          diaNames  = S.unions . map getDiagramNames $ cs'
          blocks    = filter (any (`S.member` diaNames) . codeBlockBindings) allBlocks
      in  Right $ ParsedModule m cs' blocks

-- | Turn a 'ParsedModule' back into a String.
displayModule :: ParsedModule -> String
displayModule (ParsedModule m cs _) = exactPrint m (map collapseComment cs)

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
compileDiagram :: FilePath   -- ^ cache directory
               -> FilePath   -- ^ output directory
               -> [CodeBlock] -> DiagramURL -> IO DiagramURL
compileDiagram cacheDir outputDir code url = do
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True cacheDir

  let baseFile = diagramName url <.> "svg"
      outFile  = outputDir </> baseFile

      w = read <$> M.lookup "width" (diagramOpts url)
      h = read <$> M.lookup "height" (diagramOpts url)

  res <- buildDiagram
           SVG
           zeroV
           (SVGOptions (mkSizeSpec w h))
           (map codeBlockCode code)
           (diagramName url)
           []
           [ "Diagrams.Backend.SVG" ]
           (hashedRegenerate (\_ opts -> opts) cacheDir)

  case res of
    ParseErr err    -> do putStrLn ("Parse error:")
                          putStrLn err
                          return url
    InterpErr ierr  -> do putStrLn ("Interpreter error:")
                          putStrLn (ppInterpError ierr)
                          return url
    Skipped hash    -> do copyFile (mkCached hash) outFile
                          return $ url { diagramURL = baseFile }
    OK hash svg     -> do let cached = mkCached hash
                          BS.writeFile cached (renderSvg svg)
                          copyFile cached outFile
                          return $ url { diagramURL = baseFile }
 where
   mkCached base = cacheDir </> base <.> "svg"

-- | Compile all the diagrams referenced in a single comment.
compileComment :: FilePath  -- ^ cache directory
               -> FilePath  -- ^ output directory
               -> [CodeBlock] -> CommentWithURLs -> IO CommentWithURLs
compileComment cacheDir outputDir code c = do
  urls' <-
    mapM (either
           (return . Left)
           ((Right <$>) . compileDiagram cacheDir outputDir code)
         )
         (diagramURLs c)
  return $ c { diagramURLs = urls' }

-- | Compile all the diagrams referenced in an entire module.
compileDiagrams :: FilePath  -- ^ cache directory
                -> FilePath  -- ^ output directory
                -> ParsedModule -> IO ParsedModule
compileDiagrams cacheDir outputDir m = do
  comments' <- mapM (compileComment cacheDir outputDir (pmCode m))
                    (pmComments m)
  return $ m { pmComments = comments' }

-- Failed attempt at using lens:
--  m & mapMOf (pmComments . mapped) (compileComment cacheDir outputDir (m^.pmCode))

-- | Read a file, compile all the referenced diagrams, and update all
--   the diagram URLs to refer to the proper image files.  Note, this
--   /overwrites/ the file, so it's recommended to only do this on
--   files that are under version control, so you can compare the two
--   versions and roll back if 'processHaddockDiagrams' does something
--   horrible.
processHaddockDiagrams
  :: FilePath  -- ^ cache directory
  -> FilePath  -- ^ output directory
  -> FilePath  -- ^ file to be processed
  -> IO ()
processHaddockDiagrams cacheDir outputDir file = do
  src <- Strict.readFile file
  case parseModule src of
    Left  err -> putStrLn err   -- XXX FIXME should do something better?
    Right m   -> do
      m' <- compileDiagrams cacheDir outputDir m
      writeFile file (displayModule m')
