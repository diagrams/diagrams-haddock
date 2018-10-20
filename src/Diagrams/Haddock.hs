{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Haddock
-- Copyright   :  (c) 2013-2018 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Include inline diagrams code in Haddock documentation!  For
-- example, here is a green circle:
--
-- <<diagrams/src_Diagrams_Haddock_greenCircle.svg#diagram=greenCircle&width=200>>
--
-- which was literally produced by this code:
--
-- > greenCircle = circle 1
-- >             # fc purple # pad 1.1
--
-- For a much better example of the use of diagrams-haddock, see the
-- diagrams-contrib package: <http://hackage.haskell.org/package/diagrams%2Dcontrib>.
--
-- For complete documentation and examples, see
-- <https://github.com/diagrams/diagrams-haddock/blob/master/README.md>.
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

    , getDiagramNames
    , coalesceComments

      -- * Code blocks
      -- $codeblocks

    , CodeBlock(..)
    , codeBlockCode, codeBlockIdents, codeBlockBindings
    , makeCodeBlock
    , collectBindings
    , extractCodeBlocks
    , parseCodeBlocks
    , transitiveClosure

      -- * Diagram compilation
      -- $diagrams

    , compileDiagram
    , compileDiagrams
    , processHaddockDiagrams
    , processHaddockDiagrams'

      -- * Utilities

    , showParseFailure
    , CollectErrors(..)
    , failWith
    , runCE

    ) where

import           Control.Arrow               (first, (&&&), (***))
import           Control.Lens                (makeLenses, orOf, view, (%%~),
                                              (%~), (&), (.~), (^.), (^..), _2,
                                              _3, _Right)
import           Control.Monad.Writer
import qualified Data.ByteString.Base64.Lazy as BS64
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.Lazy.Char8  as BS8
import           Data.Char                   (isSpace)
import           Data.Either                 (lefts, rights)
import           Data.Function               (on)
-- import           Data.Generics.Uniplate.Data (universeBi)
import           Data.List                   (groupBy, intercalate, isPrefixOf,
                                              partition)
import           Data.List.Split             (dropBlanks, dropDelims, split,
                                              whenElt)
import qualified Data.Map                    as M
import           Data.Maybe                  (catMaybes, mapMaybe)
import qualified Data.Set                    as S
import qualified Data.Text.Lazy              as T
import qualified Data.Text.Lazy.Encoding     as T
import qualified Graphics.Svg                as G
import           Language.Haskell.Exts       hiding (loc)
import qualified Language.Haskell.Exts       as HSE
import           Language.Preprocessor.Cpphs
import           System.Console.ANSI         (setCursorColumn)
import           System.Directory            (copyFile,
                                              createDirectoryIfMissing,
                                              doesFileExist)
import           System.FilePath             (dropExtension, normalise,
                                              splitDirectories, (<.>), (</>))
import qualified System.IO                   as IO
-- import qualified System.IO.Cautious          as Cautiously
import qualified System.IO.Strict            as Strict
import           Text.Parsec
import qualified Text.Parsec                 as P
import           Text.Parsec.String

import           Diagrams.Backend.SVG        (Options (..), SVG (..))
import qualified Diagrams.Builder            as DB
import           Diagrams.Prelude            (Diagram, V2, zero)
import           Geometry.TwoD.Size          (mkSizeSpec2D)

import           Data.Data.Lens              (template)
import           Diagrams.Backend
import           Diagrams.Builder.Opts

import           Control.Monad.Catch         (catchAll)


------------------------------------------------------------
-- Utilities
------------------------------------------------------------

-- | Pretty-print a parse failure at a particular location.
showParseFailure :: SrcLoc -> String -> String
showParseFailure loc err = unlines [ prettyPrint loc, err ]

-- | A simple monad for collecting a list of error messages.  There is
--   no facility for failing as such---in this model one simply
--   generates an error message and moves on.
newtype CollectErrors a = CE { unCE :: Writer [String] a }
  deriving (Functor, Applicative, Monad, MonadWriter [String])

-- | Generate an error message and fail (by returning @Nothing@).
failWith :: String -> CollectErrors (Maybe a)
failWith err = tell [err] >> return Nothing

-- | Run a @CollectErrors@ computation, resulting in a value of type
--   @a@ along with the collection of generated error messages.
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
--   The URL is optional (the @#@, however, is required).
parseDiagramURL :: Parser DiagramURL
parseDiagramURL =
  DiagramURL
  <$> (string "<<" *> many (noneOf "#>"))
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
-- A few miscellaneous functions for dealing with comments.

-- | Get the names of all diagrams referenced from diagram URLs in the
--   given comment.
getDiagramNames :: Comment -> S.Set String
getDiagramNames (Comment _ _ s) =
  case P.parse parseDiagramURLs "" s of
    Left _     -> error "This case can never happen; see prop_parseDiagramURLs_succeeds"
    Right urls -> S.fromList . map (view diagramName) . rights $ urls

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
      , _codeBlockIdents   :: S.Set String
      , _codeBlockBindings :: S.Set String
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
    ParseOk m           -> return . Just $ CodeBlock s
                                             (collectIdents m)
                                             (collectBindings m)
    ParseFailed loc err -> failWith . unlines $
      [ file ++ ": " ++ show l ++ ":\nWarning: could not parse code block:" ]
      ++
      showBlock s
      ++
      [ "Error was:" ]
      ++
      (indent 2 . lines $ showParseFailure loc err)
  where
    parseMode = defaultParseMode
                { fixities     = Nothing
                , baseLanguage = Haskell2010
                , extensions   = [EnableExtension MultiParamTypeClasses]
                }
    indent n  = map (replicate n ' ' ++)
    showBlock b
      | length ls > 5 = indent 2 (take 4 ls ++ ["..."])
      | otherwise     = indent 2 ls
      where ls = lines b

-- | Collect the list of names bound in a module.
collectBindings :: Module l -> S.Set String
collectBindings (Module _ _ _ _ decls) = S.fromList $ mapMaybe getBinding decls
collectBindings _ = S.empty

getBinding :: Decl l -> Maybe String
getBinding (FunBind _ [])                     = Nothing
getBinding (FunBind _ (Match _ nm _ _ _ : _)) = Just $ getName nm
getBinding (PatBind _ (PVar _ nm) _ _)      = Just $ getName nm
getBinding _                                  = Nothing

getName :: Name l -> String
getName (HSE.Ident _ s)  = s
getName (Symbol _ s) = s

getQName :: QName l -> Maybe String
getQName (Qual _ _ n) = Just $ getName n
getQName (UnQual _ n) = Just $ getName n
getQName _          = Nothing

-- | Collect the list of referenced identifiers in a module.
collectIdents :: Module SrcSpanInfo -> S.Set String
collectIdents m = S.fromList . catMaybes $
                    [ getQName n
                    | (Var _ n :: Exp SrcSpanInfo) <- m ^.. template -- universeBi m
                    ]

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
    isBird = ((||) <$> (">"==) <*> ("> " `isPrefixOf`)) . dropWhile isSpace

-- | Take the contents of a Haskell source file (and the name of the
--   file, for error reporting purposes), and extract all the code
--   blocks, as well as the referenced diagram names.
parseCodeBlocks :: FilePath -> String -> CollectErrors (Maybe ([CodeBlock], S.Set String))
parseCodeBlocks file src =
  case HSE.parseFileContentsWithComments parseMode src of
    ParseFailed loc err -> failWith $ showParseFailure loc err
    ParseOk (_, cs)     -> do
      blocks <- fmap concat
                . mapM (extractCodeBlocks file)
                . coalesceComments
                $ cs
      let diaNames  = S.unions . map getDiagramNames $ cs

      return . Just $ (blocks, diaNames)
  where
    parseMode = defaultParseMode
                { fixities      = Nothing
                , parseFilename = file
                , baseLanguage  = Haskell2010
                , extensions    = [EnableExtension MultiParamTypeClasses]
                }

-- | Given an identifier and a list of CodeBlocks, filter the list of
--   CodeBlocks to the transitive closure of the "depends-on"
--   relation, /i.e./ only blocks which bind identifiers referenced in
--   blocks ultimately needed by the block which defines the desired
--   identifier.
transitiveClosure :: String -> [CodeBlock] -> [CodeBlock]
transitiveClosure ident = tc [ident]
  where
    tc _ [] = []
    tc [] _ = []
    tc (i:is) blocks =
        let (ins,outs) = partition (\cb -> i `S.member` (cb ^. codeBlockBindings)) blocks
        in  ins ++ tc (is ++ concatMap (S.toList . view codeBlockIdents) ins) outs

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
--   @name@ is the name of the diagram.  The updated URL will also
--   refer to @outDir/name.svg@, under the assumption that @outDir@
--   will be copied into the Haddock output directory. (For
--   information on how to make this copying happen, see the README:
--   <https://github.com/diagrams/diagrams-haddock/blob/master/README.md>.)
--   If for some reason you would like this scheme to be more
--   flexible/configurable, feel free to file a feature request.
compileDiagram :: Bool       -- ^ @True@ = quiet
               -> Bool       -- ^ @True@ = generate data URIs
               -> FilePath   -- ^ cache directory
               -> FilePath   -- ^ output directory
               -> FilePath   -- ^ file being processed
               -> S.Set String -- ^ diagrams referenced from URLs
               -> [CodeBlock]
               -> DiagramURL
               -> WriterT [String] IO (DiagramURL, Bool)
compileDiagram quiet dataURIs cacheDir outputDir file ds code url
    -- See https://github.com/diagrams/diagrams-haddock/issues/7 .
  | (url ^. diagramName) `S.notMember` ds = return (url, False)

    -- The normal case.
  | otherwise = do
      let outFile = outputDir </>
                    (munge file ++ "_" ++ (url ^. diagramName)) <.> "svg"

          munge   = intercalate "_" . splitDirectories . normalise . dropExtension

          w, h :: Maybe Double
          w = read <$> M.lookup "width" (url ^. diagramOpts)
          h = read <$> M.lookup "height" (url ^. diagramOpts)

          oldURL = (url, False)
          newURL content = (url & diagramURL .~ content, content /= url^.diagramURL)

          neededCode = transitiveClosure (url ^. diagramName) code

          errHeader = file ++ ": " ++ (url ^. diagramName) ++ ":\n"

      let opts = SVGOptions (round <$> mkSizeSpec2D w h) Nothing "" [] False
      res <- liftIO $ do
        createDirectoryIfMissing True cacheDir
        when (not dataURIs) $ createDirectoryIfMissing True outputDir

        logStr $ "[ ] " ++ (url ^. diagramName)
        IO.hFlush IO.stdout

        let bopts :: BuildOpts (Diagram V2)
            -- bopts = DB.mkBuildOpts SVG zero (SVGOptions (mkSizeSpec2D w h) Nothing "" [] False)
            -- bopts = DB.diaBuildOpts (SVGOptions (round <$> mkSizeSpec2D w h) Nothing "" [] False)
            bopts = DB.diaBuildOpts opts
                      & hashCache .~ Just (123, ".diagrams-cache")
                      & buildExpr .~ url ^. diagramName

            snip = Snippet
              { _snippets = map (view codeBlockCode) neededCode
              , _pragmas = ["FlexibleContexts", "GADTs", "TypeFamilies"]
              , _imports = [ "Diagrams.Backend.SVG", "Diagrams.Prelude", "Geometry" ]
              , _qimports = []
              }

             -- & DB.snippets .~ map (view codeBlockCode) neededCode
             -- & DB.imports  .~ [ "Diagrams.Backend.SVG" ]
             -- & DB.diaExpr  .~ (url ^. diagramName)
             -- & DB.decideRegen .~ (DB.hashedRegenerate (\_ opts -> opts) cacheDir)
        -- DB.buildDiagram snip bopts

        DB.buildResult snip bopts

      case res of
        -- XXX incorporate these into error reporting framework instead of printing
        DB.ParseError err    -> do
          tell [errHeader ++ "Parse error: " ++ err]
          logResult "!"
          return oldURL
        DB.InterpError ierr  -> do
          tell [errHeader ++ "Interpreter error: " ++ DB.ppInterpError ierr]
          logResult "!"
          return oldURL
        DB.Skipped hash    -> do
          let cached = mkCached (DB.showHash hash)
          when (not dataURIs) $ liftIO $ copyFile cached outFile
          logResult "."
          if dataURIs
            then do
              svgBS <- liftIO $ BS.readFile cached
              return (newURL (mkDataURI svgBS))
            else return (newURL outFile)
        DB.OK hash d     -> do
          let cached = mkCached (DB.showHash hash)
              svgBS  = G.renderBS $ view _3 $ renderDiaT opts d
          liftIO $ IO.hFlush IO.stdout
          liftIO $ BS.writeFile cached svgBS
          url' <- if dataURIs
                    then return (newURL (mkDataURI svgBS))
                    else liftIO (copyFile cached outFile >> return (newURL outFile))
          logResult "X"
          return url'
          -- return (newURL outFile)

 where
   mkCached base = cacheDir </> base <.> "svg"
   mkDataURI svg = "data:image/svg+xml;base64," ++ BS8.unpack (BS64.encode svg)

   logStr, logResult :: MonadIO m => String -> m ()
   logStr      = liftIO . when (not quiet) . putStr
   logResult s = liftIO . when (not quiet) $ do
     setCursorColumn 1
     putStrLn s

-- | Compile all the diagrams referenced in an entire module.
compileDiagrams :: Bool          -- ^ @True@ = quiet
                -> Bool          -- ^ @True@ = generate data URIs
                -> FilePath      -- ^ cache directory
                -> FilePath      -- ^ output directory
                -> FilePath      -- ^ file being processed
                -> S.Set String  -- ^ diagram names referenced from URLs
                -> [CodeBlock]
                -> [Either String DiagramURL]
                -> WriterT [String] IO ([Either String DiagramURL], Bool)
compileDiagrams quiet dataURIs cacheDir outputDir file ds cs urls = do
  urls' <- urls & (traverse . _Right)
                %%~ compileDiagram quiet dataURIs cacheDir outputDir file ds cs
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
  :: Bool       -- ^ quiet
  -> Bool       -- ^ generate data URIs?
  -> FilePath   -- ^ cache directory
  -> FilePath   -- ^ output directory
  -> FilePath   -- ^ file to be processed
  -> IO [String]
processHaddockDiagrams = processHaddockDiagrams' opts
  where
    opts = defaultCpphsOptions
         { boolopts = defaultBoolOptions { hashline = False } }

-- | Version of 'processHaddockDiagrams' that takes options for @cpphs@.
processHaddockDiagrams'
  :: CpphsOptions -- ^ Options for cpphs
  -> Bool         -- ^ quiet
  -> Bool         -- ^ generate data URIs?
  -> FilePath     -- ^ cache directory
  -> FilePath     -- ^ output directory
  -> FilePath     -- ^ file to be processed
  -> IO [String]
processHaddockDiagrams' opts quiet dataURIs cacheDir outputDir file = do
  e   <- doesFileExist file
  case e of
    False -> return ["Error: " ++ file ++ " not found."]
    True  -> do
      -- always assume UTF-8, to make our lives simpler!
      h <- IO.openFile file IO.ReadMode
      IO.hSetEncoding h IO.utf8
      src <- Strict.hGetContents h
      r <- go src
      case r of
        (Nothing,       msgs) -> return msgs
        (Just (cs, ds), msgs) ->
          case P.parse parseDiagramURLs "" src of
            Left _     ->
              error "This case can never happen; see prop_parseDiagramURLs_succeeds"
            Right urls -> do
              ((urls', changed), msgs2) <- runWriterT $
                compileDiagrams quiet dataURIs cacheDir outputDir file ds cs urls
              let src' = displayDiagramURLs urls'

              -- See https://github.com/diagrams/diagrams-haddock/issues/8:
              -- Cautiously.writeFile truncates chars to 8 bits.  So
              -- we do the encoding to UTF-8 ourselves and then call
              -- writeFileL.
              -- when changed $ Cautiously.writeFileL file (T.encodeUtf8 . T.pack $ src')
              when changed $ writeFile file src'
              return (msgs ++ msgs2)
  where
    go src | needsCPP src = runCpp src >>= return . runCE . parseCodeBlocks file
           | otherwise    = return $ runCE (parseCodeBlocks file src)

    needsCPP src = case readExtensions src of
                     Just (_, es) | EnableExtension CPP `elem` es -> True
                     _                                            -> False

    runCpp s = runCpphs opts file s
