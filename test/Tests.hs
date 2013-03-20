module Main where

import           Control.Applicative
import           Control.Lens                         (view)
import           Data.Either                          (rights)
import           Data.List                            ((\\))
import qualified Data.Map                             as M
import qualified Data.Set                             as S
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

instance Arbitrary CodeBlock where
  arbitrary = CodeBlock <$> arbitrary <*> arbSet <*> arbSet
    where arbSet = S.fromList <$> arbitrary

prop_tc_subset :: String -> [CodeBlock] -> Bool
prop_tc_subset s blocks = all (`elem` blocks) tc
  where tc = transitiveClosure s blocks

-- excluded blocks don't bind anything the included blocks need
prop_tc_excluded :: String -> [CodeBlock] -> Bool
prop_tc_excluded s blocks = S.null (excludedBindings `S.intersection` includedIdents)
  where included = transitiveClosure s blocks
        excluded = blocks \\ included
        excludedBindings = S.unions (map (view codeBlockBindings) excluded)
        includedIdents   = S.unions (map (view codeBlockIdents)   included)

-- included blocks do bind something which included blocks need
prop_tc_included :: String -> [CodeBlock] -> Bool
prop_tc_included s blocks =
    all ( not
        . S.null
        . (S.intersection includedIdents)
        . (view codeBlockBindings)
        )
      included
  where included       = transitiveClosure s blocks
        includedIdents = S.insert s $ S.unions (map (view codeBlockIdents) included)

tests =
  [ testProperty "DiagramURL display/parse"      prop_parseDisplay
  , testProperty "CommentWithURLs display/parse" prop_parseDisplayMany
  , testProperty "parseDiagramURLs succeeds"     prop_parseDiagramURLs_succeeds

  , testProperty "transitiveClosure subset"         prop_tc_subset
  , testProperty "transitiveClosure excluded bindings" prop_tc_excluded
  , testProperty "transitiveClosure included bindings" prop_tc_included
  ]

main = defaultMain tests
