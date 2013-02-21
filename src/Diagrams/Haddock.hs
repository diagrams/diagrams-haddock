module Diagrams.Haddock where

import           Control.Applicative hiding ((<|>), many)
import qualified Data.ByteString.Lazy as BS
import           Data.Either
import           Data.List                  ( isPrefixOf, intercalate )
import           Data.List.Split            ( split, dropBlanks, dropDelims, whenElt )
import qualified Data.Map    as M
import           Data.Maybe                 ( mapMaybe   )
import           Data.Monoid
import qualified Data.Set    as S
import           Data.VectorSpace           ( zeroV )
import           Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts.Annotated as HSE
import           System.Directory (createDirectoryIfMissing, copyFile)
import           System.FilePath
import           Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import           Text.Parsec
import qualified Text.Parsec as P
import           Text.Parsec.String

import           Diagrams.Backend.SVG
import           Diagrams.Builder
import           Diagrams.TwoD.Size         ( mkSizeSpec )

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
parseDiagramURL' :: Parser (Either Char DiagramURL)
parseDiagramURL' =
      Right <$> try parseDiagramURL
  <|> Left  <$> anyChar

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

-- | Decompose a string into a parsed form with explicitly represented
--   diagram URLs interspersed with other content.
parseDiagramURLs :: Parser [Either String DiagramURL]
parseDiagramURLs = condenseLefts <$> many parseDiagramURL'
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

-- | \"Collapse\" a parsed comment back down into a normal comment.
collapseComment :: CommentWithURLs -> Comment
collapseComment (CommentWithURLs (Comment b s _) urls)
  = Comment b s (displayDiagramURLs urls)

-- | A @CodeBlock@ represents a portion of a comment which is a valid
--   code block (set off by > bird tracks).  It also caches the list
--   of bindings present in the code block.
data CodeBlock
    = CodeBlock
      { codeBlockCode     :: String
      , codeBlockBindings :: [String]
      }
  deriving (Show, Eq)

-- | Given a @String@ representing a code block, i.e. valid Haskell
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
  . map (makeCodeBlock . concat . map (drop 2))
  . (split . dropBlanks . dropDelims $ whenElt (not . ("> " `isPrefixOf`)))
  . lines

-- XXX FIXME: Need to consider consecutive single-line comments as a
-- single unit when extracting code blocks

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
          allBlocks = concatMap extractCodeBlocks . map getComment $ cs
          diaNames  = S.unions . map getDiagramNames $ cs'
          blocks    = filter (any (`S.member` diaNames) . codeBlockBindings) allBlocks
      in  Right $ ParsedModule m cs' blocks

-- | Extract the @String@ part of a @Comment@.
getComment :: Comment -> String
getComment (Comment _ _ c) = c

-- | Given a directory for cached diagrams and a directory for
--   outputting final diagrams, and all the relevant code blocks,
--   compile the diagram referenced by a single URL, returning a new
--   URL updated to point to the location of the generated diagram.
compileDiagram :: FilePath -> FilePath -> [CodeBlock] -> DiagramURL -> IO DiagramURL
compileDiagram cacheDir outputDir code url = do
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True cacheDir

  let outFile = outputDir </> diagramName url <.> "svg"

      w = read <$> M.lookup "width" (diagramOpts url)
      h = read <$> M.lookup "height" (diagramOpts url)

  res <- buildDiagram
           SVG
           zeroV
           (SVGOptions (mkSizeSpec w h))
           (map codeBlockCode code)
           (diagramName url)
           []
           []
           (hashedRegenerate (\_ opts -> opts) cacheDir)

  case res of
    ParseErr err    -> do putStrLn ("Parse error:")
                          putStrLn err
                          return url
    InterpErr ierr  -> do putStrLn ("Interpreter error:")
                          putStrLn (ppInterpError ierr)
                          return url
    Skipped hash    -> do copyFile (mkCached hash) outFile
                          return $ url { diagramURL = outFile }
    OK hash svg     -> do let cached = mkCached hash
                          BS.writeFile cached (renderSvg svg)
                          copyFile cached outFile
                          return $ url { diagramURL = outFile }
 where
   mkCached base = cacheDir </> base <.> "svg"

-- | Compile all the diagrams referenced in a single comment.
compileComment :: FilePath -> FilePath -> [CodeBlock] -> CommentWithURLs -> IO CommentWithURLs
compileComment cacheDir outputDir code c = do
  urls' <-
    mapM (either
           (return . Left)
           ((Right <$>) . compileDiagram cacheDir outputDir code)
         )
         (diagramURLs c)
  return $ c { diagramURLs = urls' }

-- | Compile all the diagrams referenced in an entire module.
compileDiagrams :: FilePath -> FilePath -> ParsedModule -> IO ParsedModule
compileDiagrams cacheDir outputDir m = do
  comments' <- mapM (compileComment cacheDir outputDir (pmCode m))
                    (pmComments m)
  return $ m { pmComments = comments' }


