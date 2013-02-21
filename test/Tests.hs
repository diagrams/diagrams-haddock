module Main where

import Control.Applicative
import Data.Either                          ( rights       )
import Test.Framework                       ( defaultMain  )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck
import Text.Parsec

import Diagrams.Haddock

newtype EString = EString { getEString :: String }
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

instance Arbitrary CommentWithURLs where
  arbitrary =
    (CommentWithURLs . (map . left) getEString) <$> arbitrary
    where
      left f (Left x)  = Left (f x)
      left _ (Right x) = Right x

prop_parseDisplay :: DiagramURL -> Bool
prop_parseDisplay d
  = case parse parseDiagramURL "" (displayDiagramURL d) of
      Left _   -> False
      Right d' -> d == d'

prop_parseDisplayMany :: CommentWithURLs -> Bool
prop_parseDisplayMany c
  = case parse parseDiagramURLs  "" (displayCommentWithURLs c) of
      Left _   -> False
      Right c' ->   rights (getCommentWithURLs c) == rights (getCommentWithURLs c')
                 && displayCommentWithURLs c == displayCommentWithURLs c

tests =
  [ testProperty "DiagramURL display/parse"      prop_parseDisplay
  , testProperty "CommentWithURLs display/parse" prop_parseDisplayMany
  ]

main = defaultMain tests