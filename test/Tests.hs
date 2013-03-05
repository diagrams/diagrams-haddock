module Main where

import           Control.Applicative
import           Data.Either                          (rights)
import qualified Data.Map                             as M
import           Language.Haskell.Exts.Annotated
import           Test.Framework                       (defaultMain)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck
import qualified Text.Parsec                          as P

import           Diagrams.Haddock

newtype EString = EString { getEString :: String }
  deriving (Eq, Show)
instance Arbitrary EString where
  arbitrary = do
    NonEmpty s <- arbitrary
    if any (`elem` "#<>&=") s
      then arbitrary
      else return (EString s)

both :: (a -> b) -> (a, a) -> (b, b)
both g (x,y) = (g x, g y)

instance Arbitrary DiagramURL where
  arbitrary = DiagramURL <$> s <*> s <*> opts
    where
      s    = getEString <$> arbitrary
      opts = (M.fromList . (map . both) getEString) <$> arbitrary

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

-- this is a bit of tomfoolery, if parseDiagramURLs does fail it
-- probably fails on something very particular and hard to stumble on
-- by chance.
prop_parseDiagramURLs_succeeds :: String -> Bool
prop_parseDiagramURLs_succeeds s
  = case P.parse parseDiagramURLs "" s of
      Left _  -> False
      Right _ -> True

tests =
  [ testProperty "DiagramURL display/parse"      prop_parseDisplay
  , testProperty "CommentWithURLs display/parse" prop_parseDisplayMany
  , testProperty "parseDiagramURLs succeeds"     prop_parseDiagramURLs_succeeds
  ]

main = defaultMain tests
