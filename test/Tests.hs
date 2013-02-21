module Main where

import Control.Applicative
import Data.Either                          ( rights       )
import Language.Haskell.Exts.Annotated
import Test.Framework                       ( defaultMain  )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck
import qualified Text.Parsec as P

import Diagrams.Haddock

newtype EString = EString { getEString :: String }
  deriving (Eq, Show)
instance Arbitrary EString where
  arbitrary = do
    NonEmpty s <- arbitrary
    if any (`elem` "#<>") s
      then arbitrary
      else return (EString s)

instance Arbitrary DiagramURL where
  arbitrary = DiagramURL <$> s <*> s
    where
      s = getEString <$> arbitrary

instance Arbitrary SrcSpan where
  arbitrary = do
    NonEmpty fileName  <- arbitrary
    Positive startLine <- arbitrary
    NonNegative startCol  <- arbitrary
    NonNegative numLines  <- arbitrary
    NonNegative numCols   <- arbitrary

    return $ SrcSpan fileName startLine startCol (startLine + numLines) (startCol + numCols)

instance Arbitrary Comment where
  arbitrary = Comment <$> arbitrary <*> arbitrary <*> arbitrary

{-
instance Arbitrary CommentWithURLs where
  arbitrary =
    (CommentWithURLs . (map . left) getEString) <$> arbitrary
    where
      left f (Left x)  = Left (f x)
      left _ (Right x) = Right x
-}

prop_parseDisplay :: DiagramURL -> Bool
prop_parseDisplay d
  = case P.parse parseDiagramURL "" (displayDiagramURL d) of
      Left _   -> False
      Right d' -> d == d'

prop_parseDisplayMany :: [Either EString DiagramURL] -> Bool
prop_parseDisplayMany c
  = case P.parse parseDiagramURLs  "" (displayDiagramURLs c') of
      Left _   -> False
      Right cp ->   rights c' == rights cp
                 && displayDiagramURLs c' == displayDiagramURLs cp
  where
    c' = (map . left) getEString c
    left f (Left x)  = Left (f x)
    left _ (Right x) = Right x

prop_explode_collapse :: Comment -> Bool
prop_explode_collapse c = c == collapseComment (explodeComment c)

tests =
  [ testProperty "DiagramURL display/parse"      prop_parseDisplay
  , testProperty "CommentWithURLs display/parse" prop_parseDisplayMany
  , testProperty "Comment explode/collapse"      prop_explode_collapse
  ]

main = defaultMain tests