module Main where

import Control.Applicative
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

prop_parseDisplay :: DiagramURL -> Bool
prop_parseDisplay d
  = case parse parseDiagramURL "" (displayDiagramURL d) of
      Left _   -> False
      Right d' -> d == d'

tests =
  [ testProperty "DiagramURL display/parse" prop_parseDisplay
  ]

main = defaultMain tests