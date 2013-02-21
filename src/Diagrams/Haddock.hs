module Diagrams.Haddock where

import Control.Applicative hiding ((<|>), many)
import Data.Either
import Data.Monoid
import Text.Parsec
import qualified Text.Parsec as P
import Text.Parsec.String

import Language.Haskell.Exts.Annotated

-- | An abstract representation of inline Haddock image URLs with
--   diagrams tags, like @<<URL#diagram:name>>@.
data DiagramURL = DiagramURL { diagramURL :: String, diagramName :: String }
  deriving (Show, Eq)

-- | Display a diagram URL in the format @<<URL#diagram:name>>@.
displayDiagramURL :: DiagramURL -> String
displayDiagramURL d = "<<" ++ diagramURL d ++ "#diagram:" ++ diagramName d ++ ">>"

-- | Parse things of the form @<<URL#diagram:name>>@.
parseDiagramURL :: Parser DiagramURL
parseDiagramURL =
  DiagramURL
  <$> (string "<<" *> many1 (noneOf "#>"))
  <*> (char '#' *> string "diagram:" *> many1 (noneOf ">") <* string ">>")

-- | Parse a diagram URL /or/ a single character which is not the
--   start of a diagram URL.
parseDiagramURL' :: Parser (Either Char DiagramURL)
parseDiagramURL' =
      Right <$> try parseDiagramURL
  <|> Left  <$> anyChar

-- | The @CommentWithURLs@ type represents a Haddock comment
--   potentially containing diagrams URLs, but with the URLs separated
--   out so they are easy to query and modify; ultimately the whole
--   thing can be turned back into a Comment.
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
